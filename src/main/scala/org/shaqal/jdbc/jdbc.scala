package org.shaqal.jdbc

import scala.util._
import scala.collection.Factory

import java.sql._

import org.shaqal._

object Jdbc:

  def queryOption[D, R](sql: Sql)(f: ResultSet => R)(using connector: Connector[D]): Try[List[Option[R]]] =
    executeQuery[D, Option[R]](sql) { rs => 
      if rs.next() then 
        connector.onRow(sql, rs)
        Some(f(rs))
      else 
        None
    }

  def queryColl[D, R, Coll[_]]
    (factory: Factory[R, Coll[R]])
    (sql: Sql)
    (f: ResultSet => R)
    (using connector: Connector[D]): 
    Try[List[Coll[R]]] =
      val builder = factory.newBuilder
      executeQuery[D, Coll[R]](sql) { rs => 
        while rs.next()
        do
          connector.onRow(sql, rs)
          builder += f(rs)
        builder.result
      }

  def executeQuery[D, A](sql: Sql)(f: ResultSet => A)(using connector: Connector[D]): Try[List[A]] =
    try
      val connection = connector.getConnection()
      try
        connector.onStatement(sql)
        val ps = connection.prepareStatement(sql.render)
        try
          val list = sql.parameterLists match
            case xs if xs.nonEmpty => 
              xs.toList map { parameterList => 
                connector.onParameterList(parameterList)
                setParameters(ps, parameterList)
                val rs = ps.executeQuery()
                f(rs)
              }          
            case _ =>
              val rs = ps.executeQuery()
              List(f(rs))
          Success(list)
        catch
          case ex: Exception =>
            connector.onExecuteError(ex)
            Failure(ex)
        finally 
          ps.close()
      catch
        case ex: Exception =>
          connector.onPrepareError(ex)
          Failure(ex)
      finally 
        connector.close(connection)
    catch
      case ex: Exception =>
        connector.onConnectionError(ex)
        Failure(ex)

  def executeUpdate[D](sql: Sql)(using connector: Connector[D]): Try[List[Int]] =
    try
      val connection = connector.getConnection()
      try
        connector.onStatement(sql)
        val ps = connection.prepareStatement(sql.render)
        try
          sql.parameterLists match
            case xs if xs.nonEmpty => Try {
              xs.toList map { parameterList => 
                connector.onParameterList(parameterList)
                setParameters(ps, parameterList)
                ps.executeUpdate()
              }
            }
            case _ =>
              Try(List(ps.executeUpdate()))
        catch
          case ex: Exception =>
            connector.onExecuteError(ex)
            Failure(ex)
        finally 
          ps.close()
      catch
        case ex: Exception =>
          connector.onPrepareError(ex)
          Failure(ex)
      finally 
        connector.close(connection)
    catch
      case ex: Exception =>
        connector.onConnectionError(ex)
        Failure(ex)

end Jdbc
