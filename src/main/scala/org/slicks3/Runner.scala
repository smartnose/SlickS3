package org.slicks3

import shapeless.{::, Default, HNil, LabelledGeneric, Poly1, Witness}
import shapeless.labelled.FieldType

case class Person(name: String = "test", age: Int = 20)


object Runner {
  def main(args: Array[String]): Unit = {

    val person = Person("John", 20)

    // val encoded = fields.map(Convert2StringSeq)
    // println(encoded)
    import DefaultEncoders._
    println(PathEncoder.encode(person))

    import PathPrefixSyntax._
    println(PathPrefixSyntax.apply[Person])
    val list = PathPrefixSyntax.apply[Person]
   // println(list.type)

    val head = PathPrefixBuilder.head(list)
    println(head)

    import shapeless.ops.hlist.IsHCons._
    val pinnedHead = PathPrefixBuilder.pin(list, "john")(hlistIsHCons)
    println(pinnedHead)

    val initial = Default.AsOptions[Person].apply()
    println(initial)
    val start = PinnedList(HNil, initial)
    import PathPrefixBuilder._
    val result = PathPrefixBuilder.pinRecursive(start, "test")
    println(result)

    val listRecord = AsListRecord[Person].apply()
    println(listRecord)

    import shapeless.record._
    import shapeless.syntax.singleton._
    import shapeless._
    println(listRecord('name))
  }
}