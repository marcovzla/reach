package edu.arizona.sista.assembly

// shared by all of the "post-mention" containers
trait Category

///////////////////////////////
// Entities
///////////////////////////////

trait Entity extends Category

case class SimpleEntity(id: String, modifications: Set[String]) extends Entity {

  // Copy constructor
  def copy(
            id: String = this.id,
            modifications: Set[String] = this.modifications
            ): SimpleEntity =  SimpleEntity(id, modifications)

}

object SimpleEntity {
  // alternate constructor
  def apply(id: String): SimpleEntity = new SimpleEntity(id, Set.empty[String])
}

case class ComplexEntity(entities: Set[SimpleEntity]) extends Entity {
  type I = Set[SimpleEntity]
  type O = Set[ComplexEntity]
  def getInput: I = entities
  def getOutput: O = Set(this)
}

///////////////////////////////
// Events
///////////////////////////////

trait Event extends Category {
  type I
  type O
  def getInput: I
  def getOutput: O
}

/** Participants can be either simple or complex entities **/
case class SimpleEvent(input: Set[Entity], output: Set[Entity], label: String) extends Event {

  type I = Set[Entity]
  type O = Set[Entity]
  def getInput: I = input
  def getOutput: O = output

  // Copy constructor
  def copy(
            input: Set[Entity] = this.input,
            output: Set[Entity] = this.output,
            label: String = this.label
            ): SimpleEvent =  new SimpleEvent(input, output, label)


  def isEquivalentTo(other: Category): Boolean = other match {
    case complex: ComplexEntity => this.output == complex.getOutput && this.input == complex.getInput
    // functionality to compare Binding SimpleEvent to a ComplexEntity
    case se: SimpleEvent => this == se
    case _ => false
  }

  def hasSameOutputAs(other: Category): Boolean = other match {
    case complex: ComplexEntity => this.output == complex.getOutput
    // functionality to compare Binding SimpleEvent to a ComplexEntity
    case se: SimpleEvent => this == se
    case _ => false
  }

  def inputMatches(other: Category): Boolean = other match {
    case se: SimpleEvent => this.input == se.input
    case reg: Regulation => this.input == reg.controlled
  }
}

object SimpleEvent {
  // alternate constructors
  def apply(input: Entity, output: Entity, label: String): SimpleEvent = new SimpleEvent(Set(input), Set(output), label)
  def apply(input: Set[Entity], output: Entity, label: String): SimpleEvent = new SimpleEvent(input, Set(output), label)
  def apply(input: Seq[Entity], output: Entity, label: String): SimpleEvent = new SimpleEvent(input.toSet, Set(output), label)
  def apply(input: Seq[Entity], output: Seq[Entity], label: String): SimpleEvent = new SimpleEvent(input.toSet, output.toSet, label)
  def apply(input: Entity, output: Set[Entity], label: String): SimpleEvent = new SimpleEvent(Set(input), output, label)
  // this transformation is only obvious for a simple entity
  def apply(input: SimpleEntity, label: String): SimpleEvent = label match {
    case "Binding" =>
      val output = ComplexEntity(Set(input))
      new SimpleEvent(Set(input), Set(output), label)
    case _ =>
      val output: Entity = input.copy(modifications = input.modifications ++ Set(label))
      new SimpleEvent(Set(input), Set(output), label)
  }
  def apply(input: Seq[SimpleEntity]): SimpleEvent = {
    val output = ComplexEntity(input.toSet)
    new SimpleEvent(input.toSet, Set(output), "Binding")
  }
}

case class Regulation(controller: Entity, controlled: Event, polarity: String) extends Event {
  // constrain value of polarity
  require(polarity == Regulation.positive || polarity == Regulation.negative, "Polarity must be \"Positive\" or \"Negative\"")

  type I = Set[Category]
  type O = Set[Entity]
  def getInput: I = Set(controller, controlled)
  def getOutput: O = controlled match {
    case se:SimpleEvent => se.getOutput
    // recurse until Set of Entities uncovered
    case reg:Regulation => reg.getOutput
  }
}

object Regulation {
  val positive = "Positive"
  val negative = "Negative"
}
