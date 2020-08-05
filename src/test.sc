import scala.util._ 
import org.shaqal._

class D1 extends Database
object D1 extends D1

class D2 extends Database
object D2 extends D2

class D3 extends Database
object D3 extends D3

given as MSSQLDBC[D1] = new MSSQLDBC("test-1", "mssql-test-1-db.properties")
given as MSSQLDBC[D2] = new MSSQLDBC("test-2", "mssql-test-2-db.properties")
given as MSSQLDBC[D3] = new MSSQLDBC("test-2", "mssql-test-2-db.properties")

def insert[A](name: String)(using Connector[A]) = {
  sql"insert into Name values ($name)".execute()
}

logged {
  sql"select 1".execute[D1]()
  sql"select 1".execute[D2]()
}

(D1, D2).logged {
  (D1, D2).transaction {
    for {
      res1 <- transaction { insert[D1]("Bob") }
      res2 <- transaction { insert[D2]("John"); Failure(new Exception("inner")) }
      res3 <- Failure(new Exception("outer"))
    } yield res2
  }
}



