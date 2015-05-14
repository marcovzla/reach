package edu.arizona.sista.bionlp

import java.io.File
import scala.io.Source
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.{ FileUtils, FilenameUtils }
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.mentions._
import edu.arizona.sista.odin.extern.export.reach._

object RunSystemTxt extends App {
  // use specified config file or the default one if one is not provided
  val config =
    if (args.isEmpty) ConfigFactory.load()
    else ConfigFactory.parseFile(new File(args(0))).resolve()

  val txtDir = new File(config.getString("txtDir"))
  val encoding = config.getString("encoding")
  val outputType = config.getString("outputType")

  // if nxmlDir does not exist there is nothing to do
  if (!txtDir.exists) {
    sys.error(s"${txtDir.getCanonicalPath} does not exist")
  }


  println("initializing reach ...")
  val reach = new ReachSystem

  println(s"Processing papers ...")
  // process papers in parallel
  for (file <- txtDir.listFiles.par if file.getName.endsWith(".txt")) {

    val paperId = FilenameUtils.removeExtension(file.getName)

    // Get text from file
    val fileContents = Source.fromFile(file.getCanonicalPath).getLines.mkString


    // process individual sections and collect all mentions
    val paperMentions = for {
      mention <- reach.extractFrom(fileContents, paperId, paperId)
    } yield mention

    if (outputType != "text") {             // if reach will handle output
      outputMentions(paperMentions, outputType, paperId, txtDir)
    }
    else {                                  // else dump all paper mentions to file
      val mentionMgr = new MentionManager()
      val lines = paperMentions.flatMap(mentionMgr.mentionToStrings)
      val outFile = new File(txtDir, s"${paperId}.mentions")
      println(s"writing ${outFile.getName} ...")
      FileUtils.writeLines(outFile, lines.asJavaCollection)
    }
  }

  def outputMentions(mentions:Seq[Mention], outputType:String, paperId:String, outputDir:File) = {
    val outFile = new File(outputDir, s"${paperId}.json")
    val outputter = new ReachOutput()
    println(s"writing ${outFile.getName} ...")
    outputter.toJSON(mentions, outFile)
  }

}
