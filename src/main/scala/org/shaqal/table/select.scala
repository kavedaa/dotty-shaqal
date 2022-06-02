package org.shaqal.table

import java.sql.ResultSet

import org.shaqal.*
import org.shaqal.column.*

case class Selected[A](
  table: TableBase,
  fields: A,
  selectItems: List[SelectItem]):

//    def toSql = 
  def read(using reader: Reader[A]) = reader.read(fields)

enum SelectItem:
  case Literal(s: String)
  case ColumnItem(column: Column)

trait Selectable[A]:
  def toItems(x: A): List[SelectItem]

object Selectable:

  given Selectable[String] with
    def toItems(x: String) = List(SelectItem.Literal(x))

  given [C <: Column]: Selectable[C] with
    def toItems(x: C) = List(SelectItem.ColumnItem(x))

  given Selectable[EmptyTuple] with
    def toItems(x: EmptyTuple) = Nil

  given [H : Selectable, T <: Tuple : Selectable]: Selectable[H *: T] with
    def toItems(x: H *: T) = 
      val head = summon[Selectable[H]].toItems(x.head)
      val tail = summon[Selectable[T]].toItems(x.tail)
      head ::: tail


trait SelectReadable

trait Reader[A]:
  type B
  def read(x: A)(rs: ResultSet): B

object Reader:

  given [BB, R <: Readable { type Q = BB}]: Reader[R] with
    type B = BB
    def read(x: R)(rs: ResultSet) = x.read(rs)

  // given Reader[EmptyTuple, EmptyTuple] with
  //   def read(x: EmptyTuple)(rs: ResultSet) = x

  // given Reader[H : Reader, T <: Tuple <: Reader]: Reader[H *: T] with
  //   def read(x: H *: T) = summon[Reader[]]