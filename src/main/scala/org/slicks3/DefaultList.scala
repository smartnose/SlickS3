package org.slicks3

import shapeless.{Default, DefaultSymbolicLabelling, DepFn1, HList, HNil}
import shapeless._
import shapeless.labelled.{FieldType, field}

trait AsListRecord[T] extends DepFn0 with Serializable {
  type Out <: HList
}

object AsListRecord {
  def apply[T](implicit default: AsListRecord[T]): Aux[T, default.Out] = default

  type Aux[T, Out0 <: HList] = AsListRecord[T] { type Out = Out0 }

  trait Helper[L <: HList, Labels <: HList] extends DepFn1[L] with Serializable {
    type Out <: HList
  }

  object Helper {
    def apply[L <: HList, Labels <: HList](implicit helper: Helper[L, Labels]): Aux[L, Labels, helper.Out] = helper

    type Aux[L <: HList, Labels <: HList, Out0 <: HList] = Helper[L, Labels] { type Out = Out0 }

    implicit def hnilHelper: Aux[HNil, HNil, HNil] =
      new Helper[HNil, HNil] {
        type Out = HNil
        def apply(l: HNil) = HNil
      }

    implicit def hconsSomeHelper[K <: Symbol, H, T <: HList, LabT <: HList, OutT <: HList]
    (implicit
     tailHelper: Aux[T, LabT, OutT]
    ): Aux[Some[H] :: T, K :: LabT, FieldType[K, List[H]] :: OutT] =
      new Helper[Some[H] :: T, K :: LabT] {
        type Out = FieldType[K, List[H]] :: OutT
        def apply(l: Some[H] :: T) = field[K](l.head.toList) :: tailHelper(l.tail)
      }

    implicit def hconsNoneHelper[K <: Symbol, T <: HList, LabT <: HList, OutT <: HList]
    (implicit
     tailHelper: Aux[T, LabT, OutT]
    ): Aux[None.type :: T, K :: LabT, FieldType[K, List[None.type]] :: OutT] =
      new Helper[None.type :: T, K :: LabT] {
        type Out = FieldType[K, List[None.type]] :: OutT
        def apply(l: None.type :: T) = field[K](List.empty) :: tailHelper(l.tail)
      }
  }

  implicit def asRecord[T, Labels <: HList, Options <: HList, Rec <: HList]
  (implicit
   default: Default.Aux[T, Options],
   labelling: DefaultSymbolicLabelling.Aux[T, Labels],
   helper: Helper.Aux[Options, Labels, Rec]
  ): Aux[T, Rec] =
    new AsListRecord[T] {
      type Out = Rec
      def apply() = helper(default())
    }
}