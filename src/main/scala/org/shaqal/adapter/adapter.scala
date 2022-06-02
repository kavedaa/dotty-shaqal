package org.shaqal.adapter

import org.shaqal._
import org.shaqal.column._

object Adapter:

  trait Access:

    def identifier(s: String): String

    def renderColumn(column: Column)(using format: ColumnFormat)(using path: Path): String =
      format match 
        case ColumnFormat.Name => 
          identifier(column.columnName)
        case ColumnFormat.Alias => 
          s"${path.aliasName}.${renderColumn(column)(using ColumnFormat.Name)}"
        case ColumnFormat.Full => 
          s"${renderColumn(column)(using ColumnFormat.Alias)} as ${column.aliasName}"


  end Access 

  object Access:
    trait Default extends Adapter.Access:
      def identifier(s: String) = s
    object Default extends Default    
    given Access = Default

  trait Definition:
    def dataType(sqlType: Int): String
    def identity: String
    def columnDefinitionSql(columnDefinition: ColumnDefinition): Sql

trait Adapter extends Adapter.Access with Adapter.Definition
