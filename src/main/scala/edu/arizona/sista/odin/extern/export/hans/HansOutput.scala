package edu.arizona.sista.odin.extern.export.hans

import java.io._
import java.util.Date
import edu.arizona.sista.bionlp.FriesEntry
import edu.arizona.sista.bionlp.mentions.{Grounding, Display}
import edu.arizona.sista.processors.Document

import org.json4s.native.Serialization
import edu.arizona.sista.odin._
import edu.arizona.sista.odin.extern.export.JsonOutputter

import HansOutput._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Defines classes and methods used to build and output HANS format models.
  *   Written by Mihai Surdeanu. 5/22/2015.
  *   Last Modified: Initial creation of infrastructure.
  */
class HansOutput extends JsonOutputter {
  type IDed = scala.collection.mutable.HashMap[Mention, String]
  type PropMap = scala.collection.mutable.HashMap[String, Any]
  type FrameList = scala.collection.mutable.MutableList[PropMap]  // has O(c) append

  // Constants:
  val AssumedProteins = Set("Family", "Gene_or_gene_product", "Protein", "Protein_with_site")

  // used by json output serialization:
  implicit val formats = org.json4s.DefaultFormats

  // incrementing ID for numbering entities
  protected val idCntr = new IncrementingId()


  //
  // Public API:
  //

  override def toJSON (paperId:String,
                       allMentions:Seq[Mention],
                       paperPassages:Seq[FriesEntry],
                       startTime:Date,
                       endTime:Date,
                       outFilePrefix:String): Unit = {
    val passageMap = passagesToMap(paperPassages)

    sentencesToJSON(paperId, allMentions, passageMap,
      startTime, endTime, new File(outFilePrefix + ".sentences.json"))

    entitiesToJSON(paperId, allMentions, passageMap,
      startTime, endTime, new File(outFilePrefix + ".entities.json"))
  }


  //
  // Private Methods
  //

  /** Creates a map of all FriesEntries, using chunkId as key */
  private def passagesToMap(paperPassages:Seq[FriesEntry]):Map[String, FriesEntry] = {
    val map = new mutable.HashMap[String, FriesEntry]()
    for(e <- paperPassages) map += e.chunkId -> e
    map.toMap
  }

  /** Outputs a JSON object containing all sections and sentences in this paper. */
  private def sentencesToJSON (paperId:String,
                               allMentions:Seq[Mention],
                               paperPassages:Map[String, FriesEntry],
                               startTime:Date,
                               endTime:Date,
                               outFile:File) {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime)

    // keeps track of all documents created for each entry
    val passageDocs = new mutable.HashMap[String, Document]()
    for(m <- allMentions)  {
      val chunkId = getChunkId(m)
      if(! passageDocs.contains(chunkId)) {
        passageDocs += chunkId -> m.document
      }
    }

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    // now output all passages as individual frames
    for(chunkId <- passageDocs.keySet) {
      assert(paperPassages.contains(chunkId))
      frames += mkPassage(model, paperId, paperPassages.get(chunkId).get, passageDocs.get(chunkId).get)
      frames ++= mkSentences(model, paperId, paperPassages.get(chunkId).get, passageDocs.get(chunkId).get)
    }

    // write the JSON to the given file
    writeJsonToFile(model, outFile)
  }

  /** Outputs a JSON object containing all entities extracted from this paper. */
  private def entitiesToJSON (paperId:String,
                              allMentions:Seq[Mention],
                              paperPassages:Map[String, FriesEntry],
                              startTime:Date,
                              endTime:Date,
                              outFile:File): Unit = {
    val model:PropMap = new PropMap
    addMetaInfo(model, paperId, startTime, endTime)

    // this stores all frames in this output
    val frames = new FrameList
    model("frames") = frames

    for(mention <- allMentions) {
      mention match {
        case em:TextBoundMention with Display with Grounding =>
          val cid = getChunkId(em)
          assert(paperPassages.contains(cid))
          val passageMeta = paperPassages.get(cid).get
          frames += mkEntityMention(paperId, passageMeta, em)
        case _ => // nothing to do here
      }
    }

    // write the JSON to the given file
    writeJsonToFile(model, outFile)
  }

  private def mkEntityMention(paperId:String,
                              passageMeta:FriesEntry,
                              mention:TextBoundMention with Display with Grounding): PropMap = {
    val f = startFrame(COMPONENT)
    // TODO: add "index", i.e., the sentence-local number for this mention from this component
    f("sentence") = mkSentenceId(paperId, passageMeta, mention.sentence)
    val sentStart = getSentenceStartCharacterOffset(mention.document, mention.sentence)
    f("start-pos") = mkRelativePosition(paperId, passageMeta, mention.startOffset) // TODO: should these be relative to sentence start?
    f("end-pos") = mkRelativePosition(paperId, passageMeta, mention.endOffset)
    f("text") = mention.text
    f("type") = prettifyLabel(mention.displayLabel)
    val xrefs = new FrameList
    mention.xref.foreach(r => xrefs += mkXref(r))
    f("xrefs") = xrefs
    f
  }

  private def startFrame(component:String):PropMap = {
    val f = new PropMap
    f("object-type") = "frame"
    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = component
    f("object-meta") = meta
    f
  }

  private def mkSentences(model:PropMap,
                          paperId:String,
                          passageMeta:FriesEntry,
                          passageDoc:Document): Seq[PropMap] = {
    val sents = new ListBuffer[PropMap]
    for(i <- passageDoc.sentences.indices) {
      sents += mkSentence(model, paperId, passageMeta, passageDoc, i)
    }
    sents.toList
  }
  private def mkSentence(model:PropMap,
                         paperId:String,
                         passageMeta:FriesEntry,
                         passageDoc:Document,
                         offset:Int): PropMap = {
    val sent = startFrame("BioNLPProcessor")
    sent("frame-id") = mkSentenceId(paperId, passageMeta, offset)
    sent("passage") = mkPassageId(paperId, passageMeta)
    sent("start-pos") = mkRelativePosition(paperId, passageMeta, getSentenceStartCharacterOffset(passageDoc, offset))
    sent("end-pos") = mkRelativePosition(paperId, passageMeta, getSentenceEndCharacterOffset(passageDoc, offset))
    sent("text") = passageDoc.sentences(offset).getSentenceText()
    sent
  }

  private def mkRelativePosition(paperId:String,
                                 passageMeta:FriesEntry,
                                 characterOffset:Int): PropMap = {
    val pos = new PropMap
    pos("object-type") = "relative-pos"
    pos("reference") = mkPassageId(paperId, passageMeta)
    pos("offset") = characterOffset
    pos
  }

  private def mkPassageId(paperId:String, passageMeta:FriesEntry):String = {
    s"pass-$paperId-$ORGANIZATION-$RUN_ID-${passageMeta.chunkId}"
  }

  private def mkSentenceId(paperId:String, passageMeta:FriesEntry, offset:Int):String = {
    s"${mkPassageId(paperId, passageMeta)}-$offset"
  }

  private def mkXref(xref:Grounding.Xref):PropMap = {
    val m = new PropMap
    m("object-type") = "db-reference"
    m("namespace") = xref.namespace
    m("id") = xref.id
    m
  }

  private def mkPassage(model:PropMap,
                        paperId:String,
                        passageMeta:FriesEntry,
                        passageDoc:Document): PropMap = {
    val passage = startFrame("nxml2fries")
    passage("frame-id") = mkPassageId(paperId, passageMeta)
    passage("frame-type") = "passage"
    passage("index") = passageMeta.chunkId
    passage("section-id") = passageMeta.sectionId
    passage("section-name") = passageMeta.sectionName
    passage("is-title") = passageMeta.isTitle
    assert(passageDoc.text.isDefined)
    passage("text") = passageDoc.text.get.replaceAll("\\n", " ")
    passage
  }

  /** Outputs a JSON object containing all events extracted from this paper. */
  private def eventsToJSON (allMentions:Seq[Mention], paperId:String, paperPassages:Map[String, FriesEntry], startTime:Date, endTime:Date, outFile:File): Unit = {
    val model:PropMap = new PropMap

    val mentions = allMentions.filter(allowableRootMentions)
    val mIds = assignMentionIds(mentions, new IDed)
    val frames = new FrameList              // top level list of mention maps
    mentions.foreach { mention =>
      // REACH creates a data structure for each mention, stores it in frames list:
      // val frame = beginNewFrame(mention, startTime, endTime, mIds)
      // frames += doMention(mention, mIds, frame)
    }
    model("frames") = frames
    writeJsonToFile(model, outFile)
  }

  def addMetaInfo(model:PropMap, paperId:String, startTime:Date, endTime:Date): Unit = {
    model("object-type") = "frame-collection"

    val meta = new PropMap
    meta("object-type") = "meta-info"
    meta("component") = COMPONENT
    meta("component-type") = "machine"
    meta("organization") = ORGANIZATION
    meta("doc-id") = paperId
    meta("processing-start") = startTime
    meta("processing-end") = endTime
    model("object-meta") = meta
  }

  /** Return true if the given mention is one that should be processed if it is an argument. */
  private def allowableArgumentMentions (mention:Mention): Boolean = {
    mention.isInstanceOf[EventMention] || mention.isInstanceOf[RelationMention]
  }

  /** Return true if the given mention is one that should be processed at the forest root. */
  private def allowableRootMentions (mention:Mention): Boolean = {
    mention.isInstanceOf[EventMention] || (mention.isInstanceOf[RelationMention] && (mention.label != "Protein_with_site"))
  }

  /** Assign all mentions a unique ID. */
  private def assignMentionIds (mentions:Seq[Mention], mIds:IDed): IDed = {
    mentions.foreach{ mention =>
      mIds.getOrElseUpdate(mention, idCntr.genNextId())
      assignMentionIds(mention.arguments.values.toSeq.flatten.filter(allowableArgumentMentions), mIds)
    }
    mIds
  }

  /** Convert the entire output data structure to JSON and write it to the given file. */
  private def writeJsonToFile (model:PropMap, outFile:File) = {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
    Serialization.writePretty(model, out)
    out.println()                           // add final newline which serialization omits
    out.flush()
    out.close()
  }

  private def getChunkId(m:Mention):String = {
    assert(m.document.id.isDefined)
    val did = m.document.id.get
    // the chunk id is the string following the underscore in the document ids
    val chunkId = did.substring(did.lastIndexOf("_") + 1)
    chunkId
  }

  private def getSentenceStartCharacterOffset(doc:Document, sentOffset:Int):Int = {
    doc.sentences(sentOffset).startOffsets.head
  }
  private def getSentenceEndCharacterOffset(doc:Document, sentOffset:Int):Int = {
    doc.sentences(sentOffset).endOffsets.last
  }

  private def prettifyLabel(label:String):String = label.toLowerCase.replaceAll("_", "-")
}

object HansOutput {
  val RUN_ID = "r1"
  val COMPONENT = "REACH"
  val ORGANIZATION = "UAZ"
}


/** Implements an incrementing identification string for numbering entities. */
class IncrementingId {
  protected var cntr = 0

  /** Return the current identification string. */
  def currentId (): String = { s"$cntr" }

  /** Increment counter and return new identification string. */
  def genNextId (): String = {
    cntr = cntr + 1
    currentId()
  }

  /** Increment counter and return new identification string. */
  def genNextIdWithFormat (formatString:String): String = {
    cntr = cntr + 1
    formatString.format(cntr)
  }
}
