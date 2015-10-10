package edu.arizona.sista.reach.extern.bids

import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.reach.mentions._

class BIDSOutput(docId:String, bioDocs:Seq[Document],  bidsMentions:Seq[BioMention]){

  // PREPROCESSING
  // First extract the sequence of sentences out of all the bioDocs
  val bidsSentences = bioDocs.flatMap(_.sentences)
  // Now, align the mentions to their particular sentence.
  // We need to compute offsets for each sentence
  val offsets = bioDocs.map(_.sentences.size).scanLeft(0)(_+_)

  val docIx = bioDocs.zipWithIndex.toMap

  // Map each mention to its global sentence index
  val bidsIndexedMentions = bidsMentions.map{
    mention =>
      (offsets(docIx(mention.document))+mention.sentence, mention)
  }
  ///////////////////////////////////////////////////////////

  // Serializes a sentence as a TSV string
  // Fields: Lemmas, POS, Dependency graph, Constituency tree
  private def sentence2txt(sen:Sentence):String = {
    val lemmas = sen.lemmas match {
      case Some(lemmas) => lemmas.mkString(" ")
      case None => Seq()
    }

    val pos = sen.tags match {
      case Some(tags) => tags.mkString(" ")
      case None => Seq()
    }

    val dependencies = (sen.dependencies match {
      case Some(graph) => graph.allEdges.map{
        case (incoming, outgoing, label) => s"($incoming, $outgoing, $label)"
      }
      case None => Seq()
    }).mkString("|")

    // val constituents = (sen.syntacticTree match {
    //   case Some(tree) =>
    //   case None => Seq()
    // }).mkString(" ")

    s"$docId\t$lemmas\t$pos\t$dependencies"//\t$constituents"
  }

  // Serializes a mention as a TSV string
  // Fields: Document Id, Sentence Ix, Lemma(s), Grounded Id, Labels, Start, End
  private def mention2txt(arg:(Int, BioMention)):String =  arg match {
    case (ix, mention) =>
      val sen = bidsSentences(ix)

      val lemma = (sen.lemmas match {
        case Some(lemmas) => mention.tokenInterval.map(lemmas)
        case None => Seq()
      }).mkString(" ")

      val groundingId = if(mention.isGrounded){
        mention.xref match {
          case Some(xref) => xref.printString
          case None => "N/A"
        }
      }
      else{
        "N/A"
      }

      val labels = mention.labels.mkString(" ")
      val start = mention.tokenInterval.start
      val end = mention.tokenInterval.end

      // Result value
      s"$docId\t$ix\t$lemma\t$groundingId\t$labels\t$start\t$end"
  }

  def sentenceLines = bidsSentences.map(sentence2txt)
  def mentionLines = bidsIndexedMentions.map(mention2txt)
}
