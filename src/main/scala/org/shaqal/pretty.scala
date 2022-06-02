package org.shaqal

import scala.language.implicitConversions

object Indentation:
  opaque type Size <: Int = Int
  def Size(x: Int): Size = x
  val Default = Size(2)

case class IndentedLine(s: String, level: Int):
  def render(implicit size: Indentation.Size) = " " * (size * level) + s
  def map(f: String => String) = IndentedLine(f(s), level)

abstract class Element:
  def indent(level: Int): List[IndentedLine]
  def render(implicit size: Indentation.Size = Indentation.Default) = indent(0).map(_.render) mkString "\n"

case class Indent(element: Element) extends Element:
  def indent(level: Int) = element indent (level + 1)

object Indent:
  def apply(es: Element*) = new Indent(ElementList(es.toList))

case class Line(s: String) extends Element:
  def indent(level: Int) = List(new IndentedLine(s, level))

abstract class ElementListLike extends Element:
  val es: List[Element]
  def indent(level: Int) = es.flatMap(_ indent level)
  def parens = ParenthesisElementList(es)

case class ElementList(es: List[Element]) extends ElementListLike

case class ParenthesisElementList(es: List[Element]) extends ElementListLike:
  override def indent(level: Int) = super.indent(level) match 
    case Nil => Nil
    case l :: Nil => List(l.map("(" + _ + ")"))
    case l :: ls => l.map("(" + _) :: ls.init ::: List(ls.last.map(_ + ")"))

case class CommaLines(ss: List[String]) extends ElementListLike:
  val es = ss match
    case Nil => Nil
    case s :: Nil => List(Line(s))
    case ss => (ss.init.map(s => Line(s + ","))) ++: List(Line(ss.last))

object Element:

  // conversion (String): Line = Line(_)
  // conversion (s: String): Line = Line(s)

  given Conversion[String, Line] = Line(_)
//  implicit def fromString(s: String) = Line(s)

  given Conversion[List[Element], ElementList] = ElementList(_)
//  implicit def fromList(es: List[Element]) = ElementList(es)

  def mkIndentList(es: List[Element], sep: Line): ElementList = es match
    case Nil => Nil
    case head :: Nil => es
    case head :: tail => Indent(head) :: (tail flatMap { e => List(sep, Indent(e)) })

end Element

object ElementList:
  def apply(es: Element*) = new ElementList(es.toList)
  given Conversion[ElementList, List[Element]] = _.es
//  implicit def toList(el: ElementList) = el.es

object Pretty:
  def unprettify(s: String) = s split "[ \n]" match
    case es if es.length > 1 => es.filter(_.nonEmpty) mkString " "
    case _ => s