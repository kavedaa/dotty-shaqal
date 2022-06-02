package org.shaqal.column

import java.sql._

import org.shaqal._

abstract class SmallIntColumn(val columnName: String) extends ColumnAccess:

  def sqlType = Types.SMALLINT

  type T = Short
  
  def fullDataType(typeName: String) = typeName
   
  def get(using rs: ResultSet) = rs getShort aliasName

  def set(v: Short) = ColumnParameter(this, SqlParameter.ShortParameter(v))
  def set(v: Option[Short]) = ColumnParameter(this, v map SqlParameter.ShortParameter.apply getOrElse SqlParameter.Null(sqlType))

  def auto(using rs: ResultSet) = rs getShort columnName
  def auto(colIndex: Int)(using rs: ResultSet) = rs getShort colIndex

  def valueParameter(value: Short) = SqlParameter.ShortParameter(value)
  
  def >(value: Short) = Gt(this, valueParameter(value))
  def >=(value: Short) = Gte(this, valueParameter(value))
  def <(value: Short) = Lt(this, valueParameter(value))
  def <=(value: Short) = Lte(this, valueParameter(value))

end SmallIntColumn

abstract class IntColumn(val columnName: String) extends ColumnAccess:

  def sqlType = Types.INTEGER

  type T = Int
  
  def fullDataType(typeName: String) = typeName
   
  def get(using rs: ResultSet) = rs getInt aliasName

  def set(v: Int) = ColumnParameter(this, SqlParameter.IntParameter(v))
  def set(v: Option[Int]) = ColumnParameter(this, v map SqlParameter.IntParameter.apply getOrElse SqlParameter.Null(sqlType))

  def auto(using rs: ResultSet) = rs getInt columnName
  def auto(colIndex: Int)(using rs: ResultSet) = rs getInt colIndex

  def valueParameter(value: Int) = SqlParameter.IntParameter(value)
  
  def >(value: Int) = Gt(this, valueParameter(value))
  def >=(value: Int) = Gte(this, valueParameter(value))
  def <(value: Int) = Lt(this, valueParameter(value))
  def <=(value: Int) = Lte(this, valueParameter(value))

end IntColumn

abstract class StringColumnBase(val columnName: String, val sqlType: Int, val dataTypeName: String) extends ColumnAccess:

  type T = String
  
  val length: DataLength | Int

  val dataLength = DataLength.from(length)

  def fullDataType(typeName: String) = List(Some(typeName), dataLength.render map("(" + _ + ")")).flatten.mkString
  
  def set(v: String) = ColumnParameter(this, SqlParameter.StringParameter(v))
  def set(v: Option[String]) = ColumnParameter(this, v map SqlParameter.StringParameter.apply getOrElse SqlParameter.Null(sqlType))

  def valueParameter(value: String) = new SqlParameter.StringParameter(value)

  def like(value: String) = Like(this, valueParameter(value))
  
  def get(using rs: ResultSet) = rs getString aliasName

  def auto(using rs: ResultSet) = rs getString columnName
  def auto(colIndex: Int)(using rs: ResultSet) = rs getString colIndex

end StringColumnBase

abstract class VarcharColumn(columnName: String) extends StringColumnBase(columnName, Types.VARCHAR, "varchar")  