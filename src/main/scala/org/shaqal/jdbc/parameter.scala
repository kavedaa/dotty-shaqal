package org.shaqal.jdbc

import java.sql.PreparedStatement
import java.time._

import org.shaqal._

def setParameter(ps: PreparedStatement, index: Int, parameter: SqlParameter[?]) = 
  parameter match {
    case SqlParameter.Null(sqlType) =>
      ps.setNull(index, sqlType)
    case SqlParameter.StringParameter(value) =>
      ps.setString(index, value)
    case SqlParameter.ShortParameter(value) =>
      ps.setShort(index, value)
    case SqlParameter.IntParameter(value) =>
      ps.setInt(index, value)
    case SqlParameter.LongParameter(value) =>
      ps.setLong(index, value)
    case SqlParameter.DoubleParameter(value) =>
      ps.setDouble(index, value)
    case SqlParameter.BigDecimalParameter(value) =>
      ps.setBigDecimal(index, value.bigDecimal)
    case SqlParameter.BooleanParameter(value) =>
      ps.setBoolean(index, value)
    case SqlParameter.LocalDateTimeParameter(value) =>
      ps.setTimestamp(index, java.sql.Timestamp valueOf value)
    case SqlParameter.LocalDateParameter(value) =>
      ps.setDate(index, java.sql.Date valueOf value)
    case SqlParameter.LocalTimeParameter(value) =>
      ps.setTime(index, java.sql.Time valueOf value)
  }

def setParameters(ps: PreparedStatement, parameters: Iterable[SqlParameter[?]]) = {
  parameters.zipWithIndex foreach { (parameter, index) => 
    setParameter(ps, index + 1, parameter) 
  }
}
