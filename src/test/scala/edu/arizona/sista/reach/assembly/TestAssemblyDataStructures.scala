package edu.arizona.sista.reach.assembly

import edu.arizona.sista.assembly._
import org.scalatest.{Matchers, FlatSpec}

class TestAssemblyDataStructures extends FlatSpec with Matchers {

  "A ComplexEntity" should "be equivalent to a Binding SimpleEvent when the members of the ComplexEntity == the output of the SimpleEvent" in {
    val ras = SimpleEntity("someid:1")
    val mek = SimpleEntity("someid:2")

    val complex = ComplexEntity(Set(ras, mek))
    val bindingEvent = SimpleEvent(Seq(ras, mek))

    bindingEvent.isEquivalentTo(complex) should be (true)

    val be2 = SimpleEvent(Seq(ras, mek), complex,"Binding")
    be2.isEquivalentTo(complex) should be (true)
  }

  it should "have the same \"output\" as a Binding SimpleEvent when the members of the ComplexEntity == the output of the SimpleEvent" in {
    val ras = SimpleEntity("someid:1")
    val mek = SimpleEntity("someid:2")

    val complex = ComplexEntity(Set(ras, mek))
    val bindingEvent = SimpleEvent(Seq(ras, mek))

    bindingEvent.sameOutputAs(complex) should be (true)

    val be2 = SimpleEvent(Seq(ras, mek), complex,"Binding")
    be2.hasSameOutputAs(complex) should be (true)
  }

  "Entities differing in their IDs" should "not be equivalent" in {
    val ras = SimpleEntity("someid:1")
    val mek = SimpleEntity("someid:2")

    ras should not equal (mek)
  }

  "Entities differing in their modifications" should "not be equivalent" in {
    val ras = SimpleEntity("someid:1", Set("Phosphorylation"))
    val ras2 = SimpleEntity("someid:1")

    ras should not equal (ras2)
  }
}
