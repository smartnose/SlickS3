package org.slicks3

import shapeless.{LabelledGeneric, Poly1, Witness}
import shapeless.labelled.FieldType

case class Person(name: String, age: Int)


object Runner {
  def main(args: Array[String]): Unit = {

    val person = Person("John", 20)
    val gen = LabelledGeneric[Person]
    val fields = gen.to(person).map(ConvertField)
    println(fields)
  }
}