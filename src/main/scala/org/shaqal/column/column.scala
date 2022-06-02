package org.shaqal.column

import org.shaqal._
import org.shaqal.adapter._

class Path(val xs: Seq[String]) {
  def aliasName = xs mkString "_"
}

object Path:
  given Path = new Path(Nil)

trait ColumnBase:
  val columnName: String
  def sqlType: Int
  def hasGeneratedValue = false

enum ColumnFormat:
  case Name, Alias, Full

object ColumnFormat:
  given ColumnFormat = ColumnFormat.Name

trait Column extends ColumnBase:
  def aliasName(using path: Path) = new Path(path.xs :+ columnName).aliasName
  def render(using format: ColumnFormat)(using path: Path)(using adapter: Adapter.Access): String = 
    adapter.renderColumn(this)
    
case class ColumnParameter[C](column: ColumnBase, parameter: SqlParameter[C])