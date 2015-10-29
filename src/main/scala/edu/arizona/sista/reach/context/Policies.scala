package edu.arizona.sista.reach.context

import edu.arizona.sista.reach.mentions._
import edu.arizona.sista.reach.nxml.FriesEntry

// Policy Two
class BoundedPaddingContext(vocabulary:Map[(String, String), Int],
 lines:Seq[(Seq[BioMention], FriesEntry)], manualAnn:Map[Int, Seq[(String, String)]] = Map(), bound:Int = 5 // Default bound to extend the policy
) extends Context(vocabulary, lines, manualAnn){

  protected def contextTypes = Seq("Species", "Organ", "CellType", "CellLine")

  // TODO: Do something smart to resolve ties
  protected def untie(entities:Seq[(String, String)]):Seq[(String, String)] = entities

  protected def padContext(prevStep:Seq[Int], remainingSteps:List[Seq[Int]], repetitions:Seq[Int], bound:Int):List[Seq[Int]] = {

    remainingSteps match {

      case head::tail =>
        // Group the prev step inferred row and the current by context type, then recurse
        val prevContext = prevStep map (this.inverseVocabulary(_)) groupBy (_._1)
        val currentContext = head map (this.inverseVocabulary(_)) groupBy (_._1)

        // Apply the heuristic
        // Inferred context of type "x"
        val newRepetitions = new Array[Int](repetitions.size)

        val currentStep = contextTypes.flatMap{ // Do this for each type of context. Flat Map as there could be more than one context of a type (maybe)
          contextType =>
            val stepIx = this.contextTypes.indexOf(contextType)

            if(repetitions(stepIx) < bound){
              (prevContext.lift(contextType), currentContext.lift(contextType)) match {
                // No prev, Current
                case (None, Some(curr)) =>
                  newRepetitions(stepIx) = 1
                  untie(curr)
                // Prev, No current
                case (Some(prev), None) =>
                  newRepetitions(stepIx) = repetitions(stepIx)+1
                  Seq(prev.head)
                // Prev, Current
                case (Some(prev), Some(curr)) =>
                  newRepetitions(stepIx) = 1
                  untie(curr)
                // No prev, No current
                case (None, None) =>
                  newRepetitions(stepIx) = 1
                  Nil
              }
            }
            else{
              newRepetitions(stepIx) = 1
              currentContext.lift(contextType) match {
                case Some(curr) =>
                  untie(curr)
                case None =>
                  Seq()
              }
            }

        } map (this.vocabulary(_))

        // Recurse
        currentStep :: padContext(currentStep, tail, newRepetitions, bound)


      case Nil => Nil
    }
  }
  // Apply the policy
  protected override def inferContext = padContext(Seq(), latentSparseMatrix, Seq.fill(this.contextTypes.size)(1), bound)

  protected override def extractEntryFeatures(entry:FriesEntry):Array[(String, Double)] = Array()
}

// Policy 1
class PaddingContext(vocabulary:Map[(String, String), Int], lines:Seq[(Seq[BioMention], FriesEntry)], manualAnn:Map[Int, Seq[(String, String)]]=Map()) extends BoundedPaddingContext(vocabulary, lines, manualAnn, lines.size){

}

// Policy 3
class FillingContext(vocabulary:Map[(String, String), Int],
 lines:Seq[(Seq[BioMention], FriesEntry)], manualAnn:Map[Int, Seq[(String, String)]] = Map(), bound:Int = 5) extends BoundedPaddingContext(vocabulary, lines, manualAnn, bound){

    // Override the infer context to fill the empty slots
    protected override def inferContext = {
      // Get the most common mentioned context of each type
      val defaultContexts = this.mentions.flatten.map(Context.getContextKey(_))  // Get the context keys of the mentions
        .filter(x => this.contextTypes.contains(x._1)).groupBy(_._1) // Keep only those we care about and group them by type
        .mapValues(bucket => bucket.map(this.vocabulary(_))) // Get their numeric value from the vocabulary
        .mapValues(bucket => bucket.groupBy(identity).mapValues(_.size)) // Count the occurences
        .mapValues(bucket => Seq(bucket.maxBy(_._2)._1)) // Select the most common element

      // Let the super class do its job
      val paddedContext = super.inferContext

      // Now for each line assign a default context if necessary
      paddedContext map {
        step =>
          // Existing contexts for this line
          val context = step.map(this.inverseVocabulary(_)).groupBy(_._1)
          this.contextTypes flatMap {
            ctype =>
              context.lift(ctype) match {
                case Some(x) =>
                  x map (this.vocabulary(_))
                case None =>
                  defaultContexts.lift(ctype).getOrElse(Seq())
              }
          }
      }
    }
}
