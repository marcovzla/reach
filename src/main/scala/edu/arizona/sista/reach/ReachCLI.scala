package edu.arizona.sista.reach

import java.io.File
import java.util.Date
import edu.arizona.sista.reach.extern.export.indexcards.IndexCardOutput

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.{ Try,Success,Failure }
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.odin._
import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.extern.export._
import edu.arizona.sista.reach.extern.export.fries._
import edu.arizona.sista.reach.extern.bids._
import edu.arizona.sista.reach.nxml._
import edu.arizona.sista.processors.Document

object ReachCLI extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val txtDir = new File(config.getString("txtDir"))
  val friesDir = new File(config.getString("friesDir"))
  val bidsDir = new File(config.getString("bidsDir"))
  val encoding = config.getString("encoding")
  val outputType = config.getString("outputType")
  val logFile = new File(config.getString("logFile"))

  // lets start a new log file
  if (logFile.exists) {
    FileUtils.forceDelete(logFile)
  }
  FileUtils.writeStringToFile(logFile, s"${now}\nstarting extraction ...\n")

  // if nxmlDir does not exist there is nothing to do
  if (!nxmlDir.exists) {
    sys.error(s"${nxmlDir.getCanonicalPath} does not exist")
  }

  // if friesDir does not exist create it
  if (!friesDir.exists) {
    println(s"creating ${friesDir.getCanonicalPath}")
    FileUtils.forceMkdir(friesDir)
  } else if (!friesDir.isDirectory) {
    sys.error(s"${friesDir.getCanonicalPath} is not a directory")
  }

  // if txtDir does not exist create it
  if (!txtDir.exists) {
    println(s"creating ${txtDir.getCanonicalPath}")
    FileUtils.forceMkdir(txtDir)
  } else if (!txtDir.isDirectory) {
    sys.error(s"${txtDir.getCanonicalPath} is not a directory")
  }

  // if bidsDir does not exist create it
  if (!bidsDir.exists) {
    println(s"creating ${bidsDir.getCanonicalPath}")
    FileUtils.forceMkdir(bidsDir)
  } else if (!bidsDir.isDirectory) {
    sys.error(s"${bidsDir.getCanonicalPath} is not a directory")
  }

  println("initializing reach ...")
  val reach = new ReachSystem

  println("initializing NxmlReader ...")
  val nxmlReader = new NxmlReader(
    config.getStringList("nxml2fries.ignoreSections").asScala)

  // process papers in parallel
  for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
    val paperId = FilenameUtils.removeExtension(file.getName)
    val startTime = now // start measuring time here
    val startNS = System.nanoTime

    // BIDS
    // Storage for BioNLPProcessor docs and extracted documents for THIS nxml file
    var bioDocs = new mutable.ArrayBuffer[Document]()
    var bidsMentions = new mutable.ArrayBuffer[(Int, BioMention)]()

    // Process individual sections and collect all mentions
    val entries = Try(nxmlReader.readNxml(file)) match {
      case Success(v) => v
      case Failure(e) =>
        val report = s"""
                        |==========
                        |
                        | ¡¡¡ NxmlReader error !!!
                        |
                        |paper: $paperId
            |
            |error:
            |${e.toString}
            |
            |stack trace:
            |${e.getStackTrace.mkString("\n")}
            |
            |==========
            |""".stripMargin
        FileUtils.writeStringToFile(logFile, report, true)
        Nil
    }

    val paperMentions = new mutable.ArrayBuffer[BioMention]
    for ((entry, ix) <- entries.zipWithIndex) {
      try {
        // BIDS: Annotate the document and store it
        val doc = reach.mkDoc(entry.text, entry.name, entry.chunkId)
        bioDocs += doc // Add it to the collection of docs

        // Run REACH
        paperMentions ++= reach.extractFrom(doc)

        // Now store all the entities along their document index
        bidsMentions ++= paperMentions.filter{
          case x:BioTextBoundMention => true
          case _ => false
        } map {(ix, _)}
        ////////////////////////////////////////////
      } catch {
        case e: Exception =>
          val report = s"""
                          |==========
                          |
                          | ¡¡¡ extraction error !!!
                          |
                          |paper: $paperId
              |chunk: ${entry.chunkId}
              |section: ${entry.sectionId}
              |section name: ${entry.sectionName}
              |
              |error:
              |${e.toString}
              |
              |stack trace:
              |${e.getStackTrace.mkString("\n")}
              |
              |==========
              |""".stripMargin
          FileUtils.writeStringToFile(logFile, report, true)
      }
    }

    // done processing
    val endTime = now
    val endNS = System.nanoTime

    // BIDS: Serialize the TSV files to be read by PANDAS in python
    val bidsOutput = new BIDSOutput(bioDocs, bidsMentions)
    FileUtils.writeLines(new File(bidsDir, s"$paperId.sentences"), bidsOutput.sentenceLines.asJavaCollection)
    FileUtils.writeLines(new File(bidsDir, s"$paperId.mentions"), bidsOutput.mentionLines.asJavaCollection)
    ///////////////////////////////////////////////////////////////

    try outputType match {
      case "text" =>
        val mentionMgr = new MentionManager()
        val lines = mentionMgr.sortMentionsToStrings(paperMentions)
        val outFile = new File(friesDir, s"$paperId.txt")
        println(s"writing ${outFile.getName} ...")
        FileUtils.writeLines(outFile, lines.asJavaCollection)
        FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)
      // Anything that is not text (including Fries-style output)
      case _ =>
        outputMentions(paperMentions, entries, outputType, paperId, startTime, endTime, friesDir)
        FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)
    } catch {
      case e:
        Exception =>
        val report =
          s"""
             |==========
             |
             | ¡¡¡ serialization error !!!
             |
             |paper: $paperId
              |
              |error:
              |${e.toString}
              |
              |stack trace:
              |${e.getStackTrace.mkString("\n")}
              |
              |==========
            """.stripMargin
        FileUtils.writeStringToFile(logFile, report, true)
    }
  }

  def now = new Date()

  def outputMentions(mentions:Seq[Mention],
                     paperPassages:Seq[FriesEntry],
                     outputType:String,
                     paperId:String,
                     startTime:Date,
                     endTime:Date,
                     outputDir:File) = {
    val outFile = outputDir + File.separator + paperId
    // println(s"Outputting to $outFile using $outputType")

    val outputter:JsonOutputter = outputType.toLowerCase match {
      case "fries" => new FriesOutput()
      case "indexcards" => new IndexCardOutput()
      case _ => throw new RuntimeException(s"Output format ${outputType.toLowerCase()} not yet supported!")
    }
    outputter.writeJSON(paperId, mentions, paperPassages, startTime, endTime, outFile)
  }

}
