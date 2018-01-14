package org.slicks3

import shapeless.HList

case class ForwardCursor[H <: HList, C, T <: HList](head: H, current: C, tail: T)
class ForwardOnly {

}
