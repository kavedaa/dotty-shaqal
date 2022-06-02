package org.shaqal.column

import java.sql.ResultSet

import org.shaqal._

trait Readable:
  type T          //	the raw datatype
  type F[X]       //  how it is transformed
  type Q = F[T]   //	the resulting type, in practice T or Option[T]
  def get(implicit rs: ResultSet): T
  def f[X](x: => X)(implicit rs: ResultSet): F[X]
  def read(implicit rs: ResultSet): Q = f(get)

trait Writable:
  type T
  type Q
  def set(v: T): ColumnParameter[T]
  def set(v: Option[T]): ColumnParameter[T]
  def :=(q: Q): ColumnParameter[T]

sealed trait Accessable extends Readable with Writable

object Accessable:
  trait NotNull extends Accessable:
    type F[X] = X
    def f[X](x: => X)(implicit rs: ResultSet): F[X] = x
    def :=(q: Q) = set(q)

  trait Nullable extends Accessable:
    type F[X] = Option[X]
    def checkNull(implicit rs: ResultSet): Boolean
    def f[X](x: => X)(implicit rs: ResultSet): F[X] = if (checkNull) None else Some(x)
    def :=(q: Q) = set(q)

trait ColumnAccess extends Column with Accessable:
  def checkNull(implicit rs: ResultSet) = ((rs getObject aliasName) == null)
  def valueParameter(value: T): SqlParameter[T]
  def is(value: T) = Eq(this, valueParameter(value))
  def isnt(value: T) = Ne(this, valueParameter(value))
  def in(values: Seq[T]) = if (values.isEmpty) False else In(this, values map valueParameter)
  def auto(implicit rs: ResultSet): T
  def auto(colIndex: Int)(implicit rs: ResultSet): T

