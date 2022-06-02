package org.shaqal

import scala.util._
import java.sql._

trait Connector[D]:

  given Connector[D] = this

  def name: String

  def getConnection(): Connection
  def close(conn: Connection): Unit

  def createTransactionConnector = new TransactionConnector(this, getConnection())

  def withTransaction[A](f: TransactionConnector[D] ?=> Try[A]): Try[A] =
    try 
      val transactionConnector = createTransactionConnector
      onTransactionStart()
      transactionConnector.transaction(f)
    catch
      case ex: Throwable =>
        onConnectionError(ex)
        onError(ex)
        Failure(ex)

  //  Hooks
  //  The reason we're using a connector as parameters for these is that 
  //  wrapping connectors may call them on the underlying connector

  def onStatement(sql: Sql)(using Connector[D]): Unit
  def onParameterList(parameterList: Iterable[SqlParameter[?]])(using Connector[D]): Unit
  def onRow(sql: Sql, rs: ResultSet)(using Connector[D]): Unit

  def onError(ex: Throwable)(using Connector[D]): Unit

  def onConnectionError(ex: Throwable)(using Connector[D]) = onError(ex)
  def onPrepareError(ex: Throwable)(using Connector[D]) = onError(ex)
  def onExecuteError(ex: Throwable)(using Connector[D]) = onError(ex)
  def onRowError(rs: ResultSet, ex: Throwable)(using Connector[D]) = onError(ex)

  def onTransactionStart()(using Connector[D]): Unit
  def onTransactionCommit()(using Connector[D]): Unit
  def onTransactionRollback()(using Connector[D]): Unit


abstract class ConnectorBase[D]
  extends Connector[D]:

  //  Provide default no-op implementations for the hooks

  def onStatement(sql: Sql)(using Connector[D]) = {}
  def onParameterList(parameterList: Iterable[SqlParameter[?]])(using Connector[D]) = {}
  def onRow(sql: Sql, rs: ResultSet)(using Connector[D]) = {}

  def onError(ex: Throwable)(using Connector[D]) = {}

  def onTransactionStart()(using Connector[D]) = {}
  def onTransactionCommit()(using Connector[D]) = {}
  def onTransactionRollback()(using Connector[D]) = {}

