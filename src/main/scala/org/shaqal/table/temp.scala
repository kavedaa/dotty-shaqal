package org.shaqal.table

import scala.deriving.*

import scala.compiletime.*

trait C[A]

//  works
inline given [Tup <: Tuple]: C[Tup] = new C[Tup]:
  val cs = summonAll[Tuple.Map[Tup, C]]

//  compiler error / crash
// inline given [Tup <: Tuple]: C[Tup] with
//   val cs = summonAll[Tuple.Map[Tup, C]]
