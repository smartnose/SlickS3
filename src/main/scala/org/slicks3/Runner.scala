package org.slicks3

import shapeless.{::, HNil, LabelledGeneric, Poly1, Witness}
import shapeless.labelled.FieldType

case class Person(name: String, age: Int)


object Runner {
  def main(args: Array[String]): Unit = {

    val person = Person("John", 20)
    val gen = LabelledGeneric[Person]

    val fields: Field[String]:: Field[Int] :: HNil= gen.to(person).map(Convert2Field)
    println(fields)


    import DefaultEncoders._
    val incoders = implicitly[ClassEncoder[Field[String]:: Field[Int] :: HNil]]
    implicitly[ClassEncoder[HNil]]
    implicitly[ClassEncoder[Field[Int]]]
    println(incoders.encode(fields))

    // val encoded = fields.map(Convert2StringSeq)
    // println(encoded)
  }
}