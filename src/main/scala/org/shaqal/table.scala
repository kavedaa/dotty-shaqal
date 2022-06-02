package org.shaqal

import scala.deriving.Mirror
import scala.compiletime._

import org.shaqal.table.*
import org.shaqal.column.*

trait Converter[A, B]:
  def convert(x: A): B

object Converter:
  given [A]: Converter[A, A] = identity

type Map2[Tup1 <: Tuple, Tup2 <: Tuple, F[_, _]] <: Tuple = (Tup1, Tup2) match
  case (h1 *: t1, h2 *: t2) => F[h1, h2] *: Map2[t1, t2, F]
  case _ => EmptyTuple

trait TableBase extends Columns:
  def tableName: String

  type Relation <: Tuple
  def relation: Relation

  type W[A] = Writable { type Q = A }

  type RelationTypes = Tuple.InverseMap[Relation, W]

  def select[A : Selectable](f: this.type => A) = Selected(this, f(this), summon[Selectable[A]].toItems(f(this)))  

  inline def insert[P <: Tuple](p: P)(using m: Mirror.ProductOf[P]) =
    summonInline[m.MirroredElemTypes =:= RelationTypes]
    // type Converters = Map2[m.MirroredElemTypes, RelationTypes, Converter]    
    // val converters = summonAll[Converters]
    

abstract class Table(val tableName: String) extends TableBase