package org.shaqal.column

import org.shaqal._
import org.shaqal.adapter._

sealed trait Expr:
  def and(that: Expr): Expr = if that == False then False else AndExpr(List(this, that))
  def &&(that: Expr) = and(that)
  def or(that: Expr): Expr = if that == True then True else OrExpr(List(this, that))
  def ||(that: Expr) = or(that)
  def parameters: Seq[SqlParameter[?]]

sealed trait Primitive extends Expr:
  def parameters = Nil

object True extends Primitive:
  override def and(that: Expr) = that
  override def or(that: Expr) = this

object False extends Primitive:
  override def and(that: Expr) = this
  override def or(that: Expr) = that

sealed trait BooleanExpr(val op: String) extends Expr:
  val es: Seq[Expr]
  def parameters = es.flatMap(_.parameters)
  def containsComplex = (es collect { case e: BooleanExpr => e } ).nonEmpty

case class AndExpr(es: Seq[Expr]) extends BooleanExpr("and")

case class OrExpr(es: Seq[Expr]) extends BooleanExpr("or")

sealed trait ColumnExpr extends Expr:
  val column: Column
  val op: String

sealed trait InfixColumnExpr(val op: String) extends ColumnExpr:
  val parameter: SqlParameter[?]
  def parameters = parameter :: Nil

case class Like(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr("like")
case class Eq(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr("=")
case class Ne(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr("!=")
case class Gt(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr(">")
case class Gte(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr(">=")
case class Lt(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr("<")
case class Lte(column: Column, parameter: SqlParameter[?]) extends InfixColumnExpr("<=")

trait ListInfixColumnExpr(val op: String) extends ColumnExpr

case class In(column: Column, values: Seq[SqlParameter[?]]) extends Expr:
  def parameters = values

trait PostfixColumnExpr(val op: String) extends ColumnExpr:
  def parameters = Nil

case class IsNull(column: Column) extends PostfixColumnExpr("is null")
case class IsNotNull(column: Column) extends PostfixColumnExpr("is not null")

object Expr:

  def render(expr: Expr)(using columnFormat: ColumnFormat, adapter: Adapter.Access): String = 
    val renderExpr = expr match    
      case True =>
        "1 = 1"
      case False =>
        "1 = 0"
      case b: BooleanExpr =>
        b.es map render mkString s" ${b.op} "
      case i: InfixColumnExpr =>
        s"${i.column.render} ${i.op} (?)"
      case l: ListInfixColumnExpr =>
        if (l.parameters.nonEmpty)
          s"${l.column.render} ${l.op} ${(l.parameters map (_ => "(?)") mkString ", ").parens}"
        else
          render(False)          
      case p: PostfixColumnExpr =>
        s"${p.column.render} ${p.op}"
      case In(column, values) =>
        s"${column.render} in ${values map (_ => "(?)") mkString ", "}"
    renderExpr.parens

  def pp(expr: Expr)(using columnFormat: ColumnFormat, adapter: Adapter): Element = 
    expr match
      case b: BooleanExpr if b.containsComplex =>
        (Element mkIndentList (b.es.toList map pp, Line(b.op))).parens
      case _ =>
        Line(render(expr))

  extension (x: String) def parens = s"($x)"

end Expr