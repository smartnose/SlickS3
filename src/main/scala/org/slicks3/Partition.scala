package org.slicks3

import shapeless.labelled.FieldType
import shapeless.ops.hlist.Mapper
import shapeless.{HList, LabelledGeneric, Poly1, Witness}

case class Partition[T](name: String, value: T)

case class Year(y: Int)
case class Month(m: Int)
case class Day(d: Int)
case class Hour(h: Int)
case class Minute(m: Int)

case class Field[T](name: String, value: T)



object ConvertField extends Poly1 {
  implicit def caseField[K, T](implicit wk: Witness.Aux[K]) = at[FieldType[K, T]](field => {
    wk.value -> Field(wk.value.toString.substring(1), field)
  })
}



