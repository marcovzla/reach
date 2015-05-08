package edu.arizona.sista.bionlp

import edu.arizona.sista.processors.Document
import edu.arizona.sista.struct.Interval
import edu.arizona.sista.odin._

class DarpaActions extends Actions {

  /** This action handles the creation of mentions from labels generated by the NER system.
    * Rules that use this action should run in an iteration following and rules recognizing
    * "custom" entities. This action will only create mentions if no other mentions overlap
    * with a NER label sequence.
    */
  def mkNERMentions(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions flatMap { m =>
      val candidates = state.mentionsFor(m.sentence, m.tokenInterval.toSeq)
      // do any candidates intersect the mention?
      val overlap = candidates.exists(_.tokenInterval.overlaps(m.tokenInterval))
      if (overlap) None else Some(m)
    }
  }

  /** This action handles the creation of ubiquitination EventMentions.
    * A Ubiquitination event cannot involve arguments (theme/cause) with the text Ubiquitin.
    */
  def mkUbiquitination(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.filter { m =>
      // Don't allow Ubiquitin
      !m.arguments.values.flatten.exists(_.text.toLowerCase.startsWith("ubiq")) 
    }
  }

  /** This action handles the creation of Binding EventMentions for rules using token patterns.
    * Currently Odin does not support the use of arguments of the same name in Token patterns.
    * Because of this, we have adopted the convention of following duplicate names with a
    * unique number (ex. theme1, theme2).
    * mkBinding simply unifies named arguments of this type (ex. theme1 & theme2 -> theme)
    */
  def mkBinding(mentions: Seq[Mention], state: State): Seq[Mention] = {

    mentions flatMap { m =>
      m match {
        case m: EventMention => {
          val args = m.arguments
          val themes = for {
            name <- args.keys
            if name startsWith "theme"
            theme <- args(name)
          } yield theme
          Seq(new EventMention(m.labels, m.trigger, Map("theme" -> themes.toSeq), m.sentence, m.document, m.keep, m.foundBy))
        }
        case r: RelationMention => Nil
        case _ => Nil
      }
    }
  }

}
