package org.shaqal.jdbc

import scala.util._
import java.sql._
import org.shaqal._

object Jdbc {

  def execute[D](sql: Sql)(using connector: Connector[D]): Try[Unit] = {
    connector onExecute sql
    try {
      val connection = connector.getConnection()
      try {
        val ps = connection prepareStatement sql.render
        try {
          sql.parameterLists match { 
            case xs if xs.nonEmpty => Try {
              xs foreach { parameterList => 
                connector onParameterList parameterList
                setParameters(ps, parameterList)
                ps.execute()
              }
            }
            case _ =>
              Try(ps.execute(): Unit)
          }
        }
        catch {
          case ex: SQLException =>
            connector.onExecuteError(ex)
            Failure(ex)
        }
        finally { 
          ps.close()
        }
      }
      catch {
        case ex: SQLException =>
          connector.onPrepareError(ex)
          Failure(ex)
      }
      finally { 
        connector close connection      
      }
    }
    catch {
      case ex: SQLException =>
        connector.onConnectionError(ex)
        connector.onError(ex)
        Failure(ex)
    }
  }
}
