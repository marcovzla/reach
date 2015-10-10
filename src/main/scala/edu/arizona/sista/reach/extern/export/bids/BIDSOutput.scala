package edu.arizona.sista.reach.extern.bids

import edu.arizona.sista.processors.{Document, Sentence}
import edu.arizona.sista.reach.mentions._

class BIDSOutput(bioDocs:Seq[Document],  bidsMentions:Seq[(Int, BioMention)]){

  // PREPROCESSING
  // First extract the sequence of sentences out of all the bioDocs
  val bidsSentences = bioDocs.flatMap(_.sentences)
  // Now, align the mentions to their particular sentence.
  // We need to compute offsets for each sentence
  val offsets = Seq(0) ++ bioDocs.map(_.sentences.size) // Careful, there is an extra element to the right we don't need
  // Map each mention to its global sentence index
  val bidsIndexedMentions = bidsMentions.map{
    case (docIx, mention) => (offsets(docIx)+mention.sentence, mention)
  }
  ///////////////////////////////////////////////////////////

  // Serializes a sentence as a TSV string
  private def sentence2txt(sen:Sentence):String = {
    "Sentence"
  }

  // Serializes a mention as a TSV string
  private def mention2txt(arg:(Int, BioMention)):String = {
    "Mention"
  }

  def sentenceLines = bidsSentences.map(sentence2txt)
  def mentionLines = bidsIndexedMentions.map(mention2txt)
}
