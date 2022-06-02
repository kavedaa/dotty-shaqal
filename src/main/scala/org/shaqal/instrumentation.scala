package org.shaqal

import java.sql.ResultSet

trait Instrument[R]:

  trait Instance:
    def onStatement(sql: Sql): Unit = {}
    def onParameterList(parameterList: Iterable[SqlParameter[?]]): Unit = {}
    def onRow(sql: Sql, rs: ResultSet): Unit = {}
    def result: R

  def createInstance: Instance

  case class Instrumented[A](
    result: R,
    data: A)

end Instrument

object Instrument:
  given defaultInstrument: Instrument[DefaultInstrument.Result] = DefaultInstrument

object DefaultInstrument extends Instrument[DefaultInstrument.Result]:

  case class Result(
    totalTime: Long,
    numStatements: Long,
    numParameterLists: Long,
    numRows: Long) {
    def render = Seq(
      s"Total time: $totalTime ms",
      s"Statements: $numStatements",
      s"Parameter lists: $numParameterLists",
      s"Rows: $numRows") mkString "\n"
  }

  def createInstance = new Instance:

    private val start = System.currentTimeMillis

    private var numStatements = 0L
    private var numParameterLists = 0L
    private var numRows = 0L

    override def onStatement(sql: Sql): Unit = { numStatements += 1 }
    override def onParameterList(parameterList: Iterable[SqlParameter[?]]): Unit = { numParameterLists += 1 }
    override def onRow(sql: Sql, rs: ResultSet): Unit = { numRows += 1 }

    def result = Result(
      System.currentTimeMillis - start,
      numStatements,
      numParameterLists,
      numRows)

end DefaultInstrument

class InstrumentationConnector[D](underlying: Connector[D], instrumentInstance: Instrument[?]#Instance)
  extends Connector[D]:

  export underlying.name
  export underlying.getConnection
  export underlying.close  

  def onStatement(sql: Sql)(using Connector[D]): Unit =
    instrumentInstance.onStatement(sql)
    underlying.onStatement(sql)

  def onParameterList(parameterList: Iterable[SqlParameter[?]])(using Connector[D]): Unit =
    instrumentInstance.onParameterList(parameterList)
    underlying.onParameterList(parameterList)

  def onRow(sql: Sql, rs: ResultSet)(using Connector[D]): Unit =
    instrumentInstance.onRow(sql, rs)
    underlying.onRow(sql, rs)

  export underlying.onError
  // export underlying.onConnectionError
  // export underlying.onExecuteError
  // export underlying.onPrepareError
  // export underlying.onRowError
  export underlying.onTransactionStart
  export underlying.onTransactionCommit
  export underlying.onTransactionRollback

  override def toString = s"InstrumentationConnector $name, underlying = $underlying"

def instrumented[D, R, A]
  (using instrument: Instrument[R])
  (f: Connector[D] ?=> A)
  (using connector: Connector[D]): instrument.Instrumented[A] = 
    val instance = instrument.createInstance
    val data = f(using InstrumentationConnector(connector, instance))
    instrument.Instrumented(instance.result, data)
