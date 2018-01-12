package org.slicks3

import shapeless.PolyDefns.{->, ~>}
import shapeless.labelled.{FieldType, KeyTag}
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.ops.record.Keys
import shapeless.ops.tuple.ToTraversable
import shapeless.tag.Tagged
import shapeless.{::, Const, HList, HNil, Id, LabelledGeneric, Lens, Poly1, PolyDefns, Witness}


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

trait ClassEncoder[T] {
  def encode(v: T): List[EncodedField]
}

object ClassEncoder {
  def instance[T](func: T => List[EncodedField]): ClassEncoder[T] = new ClassEncoder[T] {
    override def encode(v: T): List[EncodedField] = func(v)
  }

  def apply[T](implicit encoder: ClassEncoder[T]): ClassEncoder[T] = encoder
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

object PathEncoder {
  def encode[T, R <:HList, MR <: HList](data: T)(implicit gen: LabelledGeneric.Aux[T, R], mapper: Mapper.Aux[Convert2Field.type, R, MR], encoder: ClassEncoder[MR]): Seq[EncodedField] = {
    // val gen = LabelledGeneric[T]
    //return gen.to(data)
    val fields: MR = gen.to(data).map(Convert2Field)
    encoder.encode(fields)
  }
}

case class FieldHolder[T](name: String, value: Option[T])


trait MkFieldHolder[T] {
  type Repr
  def create: Repr
}

object PathPrefixSyntax {
  implicit def mkPrefix[T, Repr<:HList, KeysRepr<: HList]
    (implicit  gen: LabelledGeneric.Aux[T, Repr],
      keys: Keys.Aux[Repr, KeysRepr]) = new MkFieldHolder[T] {
    override type Repr = KeysRepr
    def create = keys()
  }

  def apply[T](implicit s: MkFieldHolder[T]) = s.create
}

case class PinnedList[H <: HList, T <: HList](head: H, tail: T)

object PathPrefixBuilder {
  def head[T <: HList](h:T)(implicit c: IsHCons[T]) = h.head

  def pin[V,  H <: Symbol, T<: HList](h: H::T, v: V)(implicit c: IsHCons.Aux[H::T, H, T]): Option[V] = Some(v)

  //def pin1[V,  H <: V with KeyTag[Symbol, V], T<: HList](h: H::T, v: V)(implicit c: IsHCons.Aux[H::T, H, T]): H = h.head

  def pinRecursive[V,  H <: HList, T<: HList](list: PinnedList[H, Option[V]::T], v: V)(implicit c: IsHCons.Aux[Option[V]::T, Option[V], T]):
    PinnedList[H::Option[V]::HNil, T] = PinnedList(list.head::Some(v):: HNil, list.tail.tail)
}



