package org.shaqal

import org.shaqal.column._

trait Columns:

  trait notnull extends Accessable.NotNull

  val max = DataLength.Max

  abstract class int(columnName: String)
    extends IntColumn(columnName) with ColumnDefinition

  abstract class varchar(val length:  DataLength | Int)(columnName: String)
    extends VarcharColumn(columnName) with ColumnDefinition:
      def this(name: String) = this(DataLength.None)(name)