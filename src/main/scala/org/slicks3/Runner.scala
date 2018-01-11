package org.slicks3

import shapeless.{::, HNil, LabelledGeneric, Poly1, Witness}
import shapeless.labelled.FieldType

case class Person(name: String, age: Int)


object Runner {
  def main(args: Array[String]): Unit = {

    val person = Person("John", 20)

    // val encoded = fields.map(Convert2StringSeq)
    // println(encoded)
    import DefaultEncoders._
    println(PathEncoder.encode(person))

    import PathPrefixSyntax._
    println(PathPrefixSyntax.apply[Person])

    
  }
}