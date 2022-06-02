package org.shaqal

import scala.util._

import java.sql._

class TransactionConnector[D](underlying: Connector[D], connection: Connection)
  extends Connector[D] {

  def name = s"${underlying.name}-[transaction]"

  def getConnection() = connection
  def close(conn: Connection) = {}

  def commit() = {
    connection.commit()
    onTransactionCommit()
  }

  def rollback() = {
    connection.rollback()
    onTransactionRollback()
  }

  override def createTransactionConnector = new TransactionConnector(this, connection) {
    //  Don't commit or rollback on nested transaction
    override def commit() = {}
    override def rollback() = {}
  }

  def transaction[A](f: TransactionConnector[D] ?=> Try[A]): Try[A] = {
    val prevAutoCommit = connection.getAutoCommit
    connection.setAutoCommit(false)
    try {
      f(using this)
    }
    catch {
      case ex: Throwable =>
        onError(ex)
        Failure(ex)
    }
    finally {
      connection.setAutoCommit(prevAutoCommit)  
      close(connection)
    }
  }

  export underlying.onStatement
  export underlying.onParameterList
  export underlying.onRow
  export underlying.onError
  export underlying.onTransactionStart
  export underlying.onTransactionCommit
  export underlying.onTransactionRollback

  override def toString = s"TransactionConnector $name, underlying = $underlying"
}

def transaction[D, A]
  (f: TransactionConnector[D] ?=> Try[A])
  (using connector: Connector[D]): 
  Try[A] =
    connector.withTransaction { (tc: TransactionConnector[D]) ?=>
      val result = f
      if result.isSuccess then
        tc.commit()
      else
        tc.rollback()        
      result
    }