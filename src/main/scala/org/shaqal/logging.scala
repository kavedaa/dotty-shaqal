package org.shaqal

import java.sql.ResultSet

trait Logger {

  def onExecute(sql: Sql)(using Connector[?]): Unit = {}
  def onParameterList(parameterList: Seq[SqlParameter[?]])(using Connector[?]): Unit = {}
  def onRow(sql: Sql, rs: ResultSet)(using Connector[?]): Unit = {}

  def onError(ex: Throwable)(using Connector[?]): Unit = {}

  def onConnectionError(ex: Throwable)(using Connector[?]): Unit = {}
  def onPrepareError(ex: Throwable)(using Connector[?]): Unit = {}
  def onExecuteError(ex: Throwable)(using Connector[?]): Unit = {}
  def onRowError(rs: ResultSet, ex: Throwable)(using Connector[?]): Unit = {}

  def onTransactionStart()(using Connector[?]): Unit = {}
  def onTransactionCommit()(using Connector[?]): Unit = {}
  def onTransactionRollback()(using Connector[?]): Unit = {}  
}

object Logger {

  given defaultLogger as Logger {

    override def onExecute(sql: Sql)(using connector: Connector[?]) = println(s"${connector.name}: Executing: ${sql.pp}")
    override def onParameterList(parameterList: Seq[SqlParameter[?]])(using connector: Connector[?]) = println(s"${connector.name}: Parameter list: ${parameterList.render}")
    override def onRow(sql: Sql, rs: ResultSet)(using connector: Connector[?]) = println(s"${connector.name}: Row: ${rs.getRow}")

    override def onError(ex: Throwable)(using connector: Connector[?]) = println(s"${connector.name}: Error: ${ex.getMessage}")

    override def onTransactionStart()(using connector: Connector[?]): Unit = println(s"${connector.name}: Starting transaction")
    override def onTransactionCommit()(using connector: Connector[?]): Unit = println(s"${connector.name}: Committed transaction")
    override def onTransactionRollback()(using connector: Connector[?]): Unit = println(s"${connector.name}: Rolled back transaction")
  }
}

class LoggingConnector[D](underlying: Connector[D], logger: Logger)
  extends Connector[D] {

  export underlying.name
  export underlying.getConnection
  export underlying.close  

  def onExecute(sql: Sql)(using Connector[D]) = {
    logger onExecute sql
    underlying onExecute sql
  }

  def onParameterList(parameterList: Seq[SqlParameter[?]])(using Connector[D]) = {
    logger onParameterList parameterList
    underlying onParameterList parameterList
  }

  def onRow(sql: Sql, rs: ResultSet)(using Connector[D]) = {
    logger.onRow(sql, rs)
    underlying.onRow(sql, rs)
  }

  def onError(ex: Throwable)(using Connector[D]) = {
    logger.onError(ex)
    underlying.onError(ex)
  }

  def onConnectionError(ex: Throwable)(using Connector[D]) = {
    logger.onConnectionError(ex)
    underlying.onConnectionError(ex)
  }

  def onPrepareError(ex: Throwable)(using Connector[D]) = {
    logger.onPrepareError(ex)
    underlying.onPrepareError(ex)
  }

  def onExecuteError(ex: Throwable)(using Connector[D]) = {
    logger.onExecuteError(ex)
    underlying.onExecuteError(ex)
  }

  def onRowError(rs: ResultSet, ex: Throwable)(using Connector[D]) = {
    logger.onRowError(rs, ex)
    underlying.onRowError(rs, ex)
  }

  def onTransactionStart()(using Connector[D]) = {
    logger.onTransactionStart()
    underlying.onTransactionStart()
  }

  def onTransactionCommit()(using Connector[D]) = {
    logger.onTransactionCommit()
    underlying.onTransactionCommit()
  }

  def onTransactionRollback()(using Connector[D]) = {
    logger.onTransactionRollback()
    underlying.onTransactionRollback()
  }

  override def toString = s"LoggingConnector $name, underlying = $underlying"
 }

def logged[D, A]
  (using logger: Logger)
  (f: Connector[D] ?=> A)
  (using connector: Connector[D])
  : A =
    f(using LoggingConnector(connector, logger))