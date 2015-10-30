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
import edu.arizona.sista.reach.nxml._
import edu.arizona.sista.reach.context._
import edu.arizona.sista.reach.context.Context._
import io.Source
import edu.arizona.sista.odin.extern.export.context._

object ReachCLI extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val nxmlDir = new File(config.getString("nxmlDir"))
  val txtDir = new File(config.getString("txtDir"))
  val friesDir = new File(config.getString("friesDir"))
  val contextDir = new File(config.getString("contextDir"))
  val encoding = config.getString("encoding")
  val outputType = config.getString("outputType")
  val logFile = new File(config.getString("logFile"))
  val annotationsDir = new File(config.getString("annotationsDir"))


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

  // if contextDir does not exist create it
  if (!contextDir.exists) {
    println(s"creating ${contextDir.getCanonicalPath}")
    FileUtils.forceMkdir(contextDir)
  } else if (!contextDir.isDirectory) {
    sys.error(s"${contextDir.getCanonicalPath} is not a directory")
  }

  // if txtDir does not exist create it
  if (!txtDir.exists) {
    println(s"creating ${txtDir.getCanonicalPath}")
    FileUtils.forceMkdir(txtDir)
  } else if (!txtDir.isDirectory) {
    sys.error(s"${txtDir.getCanonicalPath} is not a directory")
  }

  println("initializing reach ...")
  val reach = new ReachSystem

  println("initializing NxmlReader ...")
  val nxmlReader = new NxmlReader(
    config.getStringList("nxml2fries.ignoreSections").asScala)

  // CONTEXT
  // This is to build the context vocabulary
  val contextNames = mutable.Set[(String, String)]()
  val contextDocs = mutable.Map[String, mutable.ArrayBuffer[(Seq[BioMention], FriesEntry)]]()
  //////////////////////////////////////////////////

  // process papers in parallel
  for (file <- nxmlDir.listFiles.par if file.getName.endsWith(".nxml")) {
    val paperId = FilenameUtils.removeExtension(file.getName)
    println(s"Working on $paperId")
    val startTime = now // start measuring time here
    val startNS = System.nanoTime

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

    // Storage of context mentions
    var contextLines = new mutable.ArrayBuffer[(Seq[BioMention], FriesEntry)]

    val paperMentions = new mutable.ArrayBuffer[BioMention]
    val mentionsEntriesMap = new mutable.HashMap[BioMention, FriesEntry]()

    for (entry <- entries) {
      try {
        // Had to do this to extract the number of sentences even if no events were found in the FriesEntry
        val docEntry = reach.mkDoc(entry.text, entry.name, entry.chunkId)
        val mentions:Seq[BioMention] = reach.extractFrom(docEntry)
        //////
        mentions foreach { m => mentionsEntriesMap += (m -> entry) }
        paperMentions ++= mentions

        // Filter out all the mentions we don't care about in context
        val contextMentions = mentions filter {
          mention => (contextMatching map (mention.labels.contains(_))).foldLeft(false)(_||_) // This is a functional "Keep elements that have at least one of these"
        }

        // Add all the names to the context's names cache
        contextMentions foreach {
          contextNames += getContextKey(_)
        }
        // Unpack and store this section's context mentions
        // Group mentions by line, for each we're going to extract it's context bio-mentions
        val groupedMentions:Map[Int, Seq[BioMention]] = contextMentions.groupBy(_.sentence)

        // Append the lists to the contextLines storage
        val mentionsPerLine = groupedMentions.map{
          case (key, value) => (key, value map {(_, entry)} )
        }.toMap

        for(ix <- 0 until docEntry.sentences.size){
          contextLines += (mentionsPerLine.lift(ix) match {
            case None => (Seq(), entry)
            case Some(a) => (a map (_._1), a.head._2) //TODO: Make this legible
          })
        }

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

    contextDocs += (paperId -> contextLines)

    // done processing
    val endTime = now
    val endNS = System.nanoTime



    try outputType match {
      case "text" =>
        val mentionMgr = new MentionManager()
        val lines = mentionMgr.sortMentionsToStrings(paperMentions)
        val outFile = new File(friesDir, s"$paperId.txt")
        println(s"writing ${outFile.getName} ...")
        FileUtils.writeLines(outFile, lines.asJavaCollection)
        FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)

      case "pandas" =>

        println("Using pandas output ...")
        // Write down the context output
        val outputter:PandasOutput = new PandasOutput()

        val (entities, events, relations, lines) = outputter.toCSV(paperId, paperMentions, mentionsEntriesMap.toMap)
        // Write the text files
        val outMentions = new File(contextDir, s"$paperId.entities")
        val outEvents = new File(contextDir, s"$paperId.events")
        val outRelations = new File(contextDir, s"$paperId.relations")
        val outLines = new File(contextDir, s"$paperId.lines")
        FileUtils.writeLines(outMentions, entities.asJavaCollection)
        FileUtils.writeLines(outEvents, events.asJavaCollection)
        FileUtils.writeLines(outRelations, relations.asJavaCollection)
        FileUtils.writeLines(outLines, lines.asJavaCollection)

        FileUtils.writeStringToFile(logFile, s"Finished $paperId successfully (${(endNS - startNS)/ 1000000000.0} seconds)\n", true)

      // Anything that is not text (including Hans-style output)
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

  // CONTEXT magic!
  println("Infering context ...")

  // Extend the contextNames with annotation labels
  for((paperId, contextLines) <- contextDocs){
    val annotations:List[Array[String]] = Source.fromFile(new File(annotationsDir + s"/$paperId.tsv")).getLines.toList.map(_.split("\t"))

    val ctxAnn = annotations.map{
        _(1).split(",").map(_.trim).filter(x => !x.startsWith("E") && x != "").map{
            ann => (s"${ann(0)}", ann)
        }.toSeq
    }.toSeq

    for(ca <- ctxAnn){
        contextNames ++= ca
    }
  }
  val contextVocabulary = contextNames.zipWithIndex.toMap
  ///////////////////////////////////////////////

  // One iteration per document
  for((paperId, contextLines) <- contextDocs){
    // Hack to add the annotations to the context
    val annotations:List[Array[String]] = Source.fromFile(new File(annotationsDir + s"/$paperId.tsv")).getLines.toList.map(_.split("\t"))
    val lines = annotations.map(_(0).toInt)
    val ctxAnn = annotations.map{
        _(1).split(",").map(_.trim).filter(x => !x.startsWith("E") && x != "").map{
            ann => (s"${ann(0)}", ann)
        }.toSeq
    }.toSeq

    // ctxAnn.foreach{x => println(s"Ctx: ${x.mkString(" ")}")}
    val manualAnnotations:Map[Int, Seq[(String, String)]] = lines.zip(ctxAnn).toMap

    // Now create the context events file
    val evtAnn = annotations.map{
        _(1).split(",").map(_.trim).filter(x => x.startsWith("E") && x != "").toSeq
    }.toSeq

    // evtAnn.foreach{x => println(s"Evt: ${x.mkString(" ")}")}

    val relations = annotations.map{
        _(2).split(",").map(_.trim).filter(x => x.length > 0).toSeq
    }

    // relations.foreach{x => println(s"Rel: ${x.mkString(" ")}")}

    val event_context = evtAnn zip relations

    // Create a map of the context annotations with their line
    val ctxLines:Map[String, Int] = annotations.map{
        ann =>
            // Tuple with collapsed annotations
            (ann(0).toInt, ann(1).split(",").map(_.trim).filter(x => x != ""))
    }.filter(_._2.size > 0).flatMap{
        case (ix, ctxs) =>
            ctxs map {(_, ix)}
    }.toMap

    val events_context:Seq[String] = (0 until annotations.size).flatMap{
        ix:Int =>
            val (evts, rels) = event_context(ix)
            // Only work in lines with events
            if(evts.length > 0){
                for(rel <- rels) yield {
                    val ctxIx:Int = ctxLines.getOrElse(rel, -1)
                    val ctxId:Int = if(rel != ""){ contextVocabulary((s"${rel(0)}", rel)) } else {-1}
                    val ctxType:String = rel(0) match {
                        case 'C' => "CellType"
                        case 'T' => "TissueType"
                        case 'S' => "Species"
                        case _ => "Unspecified"
                    }
                    s"Event\t$ix\t$ctxType\t$ctxId\t$ctxIx"
                }
            }else{
                Seq("")
            }
    }.filter(_ != "")

    val eventsContextFile = new File(contextDir, s"$paperId.events_context")
    FileUtils.writeLines(eventsContextFile, events_context.asJavaCollection)
    //////////////////////////////////////////////

    // Create a Context instance to do inference and make queries

    val context = new FillingContext(contextVocabulary, contextLines.toSeq, manualAnnotations)
    outputContext(context, contextDir + File.separator + paperId)
    outputVocabularies(context, contextDir + File.separator + paperId)
  }

  /////////////////

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
