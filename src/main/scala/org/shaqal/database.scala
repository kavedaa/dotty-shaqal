package org.shaqal

import scala.util._

trait Database

object Database:

  extension[D <: Database, A, R](d: D)

    def logged
      (using logger: Logger)
      (f: Connector[D] ?=> A)
      (using connector: Connector[D])
      : A =
        f(using LoggingConnector(connector, logger))    

  extension [D1 <: Database, D2 <: Database, A, R](d: (D1, D2))

    def logged
      (using logger: Logger)
      (f: (Connector[D1], Connector[D2]) ?=> A)
      (using connector1: Connector[D1], connector2: Connector[D2]): A =
        f(using LoggingConnector[D1](connector1, logger), LoggingConnector[D2](connector2, logger))

    def instrumented
      (using instrument: Instrument[R])
      (f: (Connector[D1], Connector[D2]) ?=> A)
      (using connector1: Connector[D1], connector2: Connector[D2]): instrument.Instrumented[A] = {
        val instance = instrument.createInstance
        val data = f(using InstrumentationConnector[D1](connector1, instance), InstrumentationConnector[D2](connector2, instance))
        instrument.Instrumented(instance.result, data)
      }

    def transaction
      (f: (TransactionConnector[D1], TransactionConnector[D2]) ?=> Try[A])
      (using connector1: Connector[D1], connector2: Connector[D2])
      : Try[A] =
        connector1.withTransaction { (tc1: TransactionConnector[D1]) ?=>
          connector2.withTransaction { (tc2: TransactionConnector[D2]) ?=>
            val result = f
            if result.isSuccess then
              tc1.commit()
              tc2.commit()
            else
              tc1.rollback()        
              tc2.rollback()        
            result
          }
        }

  extension [D1 <: Database, D2 <: Database, D3 <: Database, A, R](t: (D1, D2, D3))

    def logged
      (using logger: Logger)
      (f: (Connector[D1], Connector[D2], Connector[D3]) ?=> A)
      (using connector1: Connector[D1], connector2: Connector[D2], connector3: Connector[D3]): A =
        f(using LoggingConnector[D1](connector1, logger), LoggingConnector[D2](connector2, logger), LoggingConnector[D3](connector3, logger))
