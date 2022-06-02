package org.shaqal.column

import scala.collection.mutable.ListBuffer

import org.shaqal.*
import org.shaqal.adapter.*

enum DataLength(val render: Option[String]):
  case Value(x: Int) extends DataLength(Some(x.toString))
  case Max extends DataLength(Some("max"))
  case None extends DataLength(scala.None)

object DataLength:
  def from(x: DataLength | Int) = x match
    case i: Int => DataLength.Value(i)  
    case x: DataLength => x

trait ColumnDefinition extends ColumnBase:

  def fullDataType(typeName: String): String

  def dataTypeName(using adapter: Adapter) = fullDataType(adapter dataType sqlType)

  private val elements = ListBuffer[ColumnDefinition.Element[_]]()
  def addElement(element: ColumnDefinition.Element[_]) = elements += element

  override def hasGeneratedValue = elements.exists(_.hasGeneratedValue)

//  def definitionSql(implicit adapter: Adapter) = adapter columnDefinitionSql this
end ColumnDefinition

object ColumnDefinition:

  trait Element[C]:
    val hasGeneratedValue = false
    def render(using Adapter): String
    def params: Seq[SqlParameter[?]] = Nil

  object Element:

    object NotNullable extends Element[Nothing]:
      def render(using Adapter) = "not null"

    object Nullable extends Element[Nothing]:
      def render(using Adapter) = "null"

    object Identity extends Element[Nothing]:
      override val hasGeneratedValue = true
      def render(using adapter: Adapter) = adapter.identity

    object Unique extends Element[Nothing]:
      def render(using Adapter) = "unique"

    case class Default[C](value: SqlParameter[C]) extends Element[C]:
      override val hasGeneratedValue = true
      def render(using Adapter) = "default (?)"
      override def params = List(value)

  end Element
end ColumnDefinition