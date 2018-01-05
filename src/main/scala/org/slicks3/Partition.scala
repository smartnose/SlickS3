package org.slicks3

import shapeless.PolyDefns.{->, ~>}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Mapper
import shapeless.{Const, HList, HNil, Id, LabelledGeneric, Poly1, PolyDefns, Witness, ::}

case class Partition[T](name: String, value: T)

case class Year(y: Int)
case class Month(m: Int)
case class Day(d: Int)
case class Hour(h: Int)
case class Minute(m: Int)

case class Field[T](name: String, value: T)
case class EncodedField(name: String, value: String)

object Convert2Field extends Poly1 {
  implicit def caseField[K, T](implicit wk: Witness.Aux[K]) = at[FieldType[K, T]](field => {
    Field(wk.value.toString.substring(1), field.asInstanceOf[T])
  })
}

trait Encoder[T] {
  def encode(v: T): String
}

object Encoder {
  def instance[T](f: T => String) = new Encoder[T] {
    override def encode(v: T): String = f(v)
  }
}

trait FieldEncoder[T] {
  def encode(v: Field[T]): EncodedField
}

object FieldEncoder {
  def fromEncoder[T](e: Encoder[T]) = new FieldEncoder[T] {
    override def encode(v: Field[T]): EncodedField = EncodedField(v.name, e.encode(v.value))
  }
}

/* trait DefaultTranscoders {
  implicit val stringTranscoder: Transcoder[String] = Transcoder(_ => "String")
  implicit val intTranscoder: Transcoder[Int] = Transcoder(_ => "Int")
} */



trait ClassEncoder[T] {
  def encode(v: T): List[EncodedField]
}

object ClassEncoder {
  def instance[T](func: T => List[EncodedField]): ClassEncoder[T] = new ClassEncoder[T] {
    override def encode(v: T): List[EncodedField] = func(v)
  }
}

trait HListEncoders {
  implicit val hnilEncoder: ClassEncoder[HNil] = ClassEncoder.instance(_ => Nil)
  implicit def hLsitEncoder[H, T<: HList](implicit hEncoder: ClassEncoder[H], tEncoder: ClassEncoder[T]): ClassEncoder[H::T] = ClassEncoder.instance {
    case h::t => hEncoder.encode(h) ++ tEncoder.encode(t)
  }
}

object DefaultEncoders extends HListEncoders {
  implicit def fieldEncoder2ClassEncoder[T](encoder: FieldEncoder[T]) = new ClassEncoder[Field[T]] {
    override def encode(v: Field[T]) = List(encoder.encode(v))
  }
  implicit val strFieldEncoder: FieldEncoder[String] = FieldEncoder.fromEncoder[String](Encoder.instance[String]((v: String) => v))
  implicit val intFieldEncoder: FieldEncoder[Int] = FieldEncoder.fromEncoder[Int](Encoder.instance[Int]((v: Int) => v.toString))
  implicit val intClassEncoder = fieldEncoder2ClassEncoder(intFieldEncoder)
  implicit val strClassEncoder = fieldEncoder2ClassEncoder(strFieldEncoder)

}




