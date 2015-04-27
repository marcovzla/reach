package edu.arizona.sista.bionlp.reach.ruler


import java.io.{PrintWriter, File, BufferedReader, FileReader}
import edu.arizona.sista.processors.{DocumentSerializer, Document}
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.odin._
import edu.arizona.sista.bionlp.reach.brat._

/** This class applies DARPA rules to all the files specified a command
 *  line arguments.
 *
 *  Based on the CommanLineOutout class
 *
 *  Created by Enrique on 3/17/15.
 */
object BratOutput extends App {

  val entityRules = BasicRuler.readEntityRules()
  val eventRules = BasicRuler.readEventRules()
  val rules = entityRules + "\n\n" + eventRules

  val ds = new DocumentSerializer

  val actions = new DarpaActions

  val proc = new BioNLPProcessor()
  val extractor = new BasicRuler(rules, actions)

  //val outDir = s"${System.getProperty("user.home")}/Desktop/processed_papers/"
  val outDir = s"./"

  val paperNames = args // Take the arguments from the command line

  def mkOutputName(paper: String): String = s"$outDir${ """^.*?/|.txt.ser""".r.replaceAllIn(paper, "")}.ann"

  def processPapers(papers: Seq[String]) = papers.foreach { paper => processPaper(paper)}

  def processPaper(paper: String): Unit = {

    println(s"Processing $paper...")

    val outName = mkOutputName(paper.split("/").last) // Make sure to remove the directory from the file name
    val output = new PrintWriter(new File(outName))

    println(s"Writing output to $outName")

    val doc = docFromSerializedFile(s"$paper")
    val mentions: Seq[Mention] = retrieveMentions(doc)

    // Read the original text file

    //Here do something with Brat

    val standoff = Brat.dumpStandoff(mentions, doc)
    output.write(standoff)

    output.close()
  }

  def docFromSerializedFile(filename: String): Document = {
    val br = new BufferedReader(new FileReader(filename))
    val doc = ds.load(br)
    doc
  }

  def retrieveMentions(doc: Document): Seq[Mention] = {
    val events = extractor.extractFrom(doc).filter(_.isInstanceOf[EventMention])
    val arguments = events.flatMap(e => e.arguments.values).flatMap(x => x).filter(_.isInstanceOf[TextBoundMention])

    arguments ++ events
  }

  processPapers(paperNames)
}
