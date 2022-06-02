package org.shaqal

import java.time._
import java.sql._

import scala.collection.Factory
import scala.util._

import org.shaqal._
import org.shaqal.jdbc._

sealed abstract class SqlParameter[+A] {
  def render: String
}

object SqlParameter {

  case class Null(sqlType: Int) extends SqlParameter[Nothing] {
    def render = "NULL"
  }

  sealed abstract class ValueParameter[A] extends SqlParameter[A] {
    def value: A
    def render = value.toString
  }

  case class StringParameter(value: String) extends ValueParameter[String]
  case class ShortParameter(value: Short) extends ValueParameter[Short]
  case class IntParameter(value: Int) extends ValueParameter[Int]
  case class LongParameter(value: Long) extends ValueParameter[Long]
  case class DoubleParameter(value: Double) extends ValueParameter[Double]
  case class BigDecimalParameter(value: BigDecimal) extends ValueParameter[BigDecimal]
  case class BooleanParameter(value: Boolean) extends ValueParameter[Boolean]
  case class LocalDateTimeParameter(value: LocalDateTime) extends ValueParameter[LocalDateTime]
  case class LocalDateParameter(value: LocalDate) extends ValueParameter[LocalDate]
  case class LocalTimeParameter(value: LocalTime) extends ValueParameter[LocalTime]

  def apply(value: Any) = value match {
    case x: Short => ShortParameter(x)
    case x: Int => IntParameter(x)
    case x: Long => LongParameter(x)
    case x: BigDecimal => BigDecimalParameter(x)
    case x: Boolean => BooleanParameter(x)
    case x: LocalDateTime => LocalDateTimeParameter(x)
    case x: LocalDate => LocalDateParameter(x)
    case x: LocalTime => LocalTimeParameter(x)
    case x => StringParameter(x.toString)
  }

  extension (xs: Iterable[SqlParameter[?]]) {
    def render = xs map(_.render) mkString ", "
  }
}

trait Sql {

  def render: String
  def pp: String = render
  def parameterLists: Iterable[Iterable[SqlParameter[?]]]

  def queryColl[D, R, Coll[_]](factory: Factory[R, Coll[R]])(f: ResultSet => R)(using connector: Connector[D]): Try[List[Coll[R]]] =
    Jdbc.queryColl(factory)(this)(f)

  def execute[D]()(using Connector[D]): Try[List[Int]] = 
    Jdbc.executeUpdate(this)

  override def toString = pp
}

class StatementSql(val render: String, val parameterLists: Seq[Seq[SqlParameter[?]]]) extends Sql

/**
  * If more than one of the arguments are a collection, it is assumed
  * that they have the same size. Otherwise an exeption is thrown.
  */
extension (sc: StringContext) {
  def sql(args: Any*): Sql = {
    val statement = sc.parts mkString "(?)"
    val parameters =
      if (args.nonEmpty) {
        val iterables = args collect { case i: Iterable[?] => i }
        if (iterables.nonEmpty) {
          val max = (iterables map (_.size)).max
          val iterators = args map {
            case i: Iterable[?] => i.iterator
            case x => Iterator continually x
          }
          val builder = Seq.newBuilder[Seq[SqlParameter[?]]]
          for (_ <- 0 until max) builder += iterators map { i => SqlParameter(i.next()) }
          builder.result
        }
        else Seq(args map SqlParameter.apply)
      }
      else Nil
    new StatementSql(statement, parameters)
  }
}


