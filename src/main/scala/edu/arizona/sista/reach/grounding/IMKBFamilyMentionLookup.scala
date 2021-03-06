package edu.arizona.sista.reach.grounding

import edu.arizona.sista.odin._
import edu.arizona.sista.reach.grounding.ReachKBKeyTransforms._

/**
  * Class implementing alternate key mention lookups for protein family knowledge bases.
  *   Written by Tom Hicks. 11/15/2015.
  *   Last Modified: Fix: call superclass constructor.
  */
class IMKBFamilyMentionLookup (

  /** The in-memory knowledge base that all lookups will work against. */
  memoryKB: InMemoryKB = new InMemoryKB()

) extends IMKBMentionLookup (memoryKB) {

  /** Resolve the given Mention to an optional entry in a knowledge base.
    * Return a resolution for the entry, if any found.
    */
  override def resolve (mention:Mention): Resolutions =
    resolveAlt(mention.text, familyKeyTransforms)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * for the single named species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveByASpecies (mention:Mention, species:String): Resolutions =
    resolveByASpeciesAlt(mention.text, species, familyKeyTransforms)

  /** Resolve the given Mention to an optional group of entries in a knowledge base,
    * returning resolutions for all species entries found in the KB.
    */
  override def resolveBySpecies (mention:Mention, speciesSet:SpeciesNameSet): Resolutions =
    resolveBySpeciesAlt(mention.text, speciesSet, familyKeyTransforms)

  /** Resolve the given Mention to an optional entry in a knowledge base,
    * failing if the entry is not for humans.
    * Return a resolution for a human entry, if any found.
    */
  override def resolveHuman (mention:Mention): Resolutions =
    resolveHumanAlt(mention.text, familyKeyTransforms)

  /** Resolve the given Mention to an optional entry in a knowledge base which
    * explicitly does not have an associated species. Fail if all entries have species.
    * Return a resolution for the entry, if any found.
    */
  override def resolveNoSpecies (mention:Mention): Resolutions =
    resolveNoSpeciesAlt(mention.text, familyKeyTransforms)

}
