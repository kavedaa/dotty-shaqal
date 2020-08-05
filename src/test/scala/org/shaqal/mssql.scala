package org.shaqal

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import net.sourceforge.jtds.jdbcx.JtdsDataSource

object JtdsFactory extends DataSourceFactory {
  def getDataSource(server: String, database: String, port: Int, user: String, password: String) = {
    val ds = new JtdsDataSource
    ds setServerName server
    ds setDatabaseName database
    ds setPortNumber port
    ds setUser user
    ds setPassword password
    ds
  }
}

class HikariFactory(underlying: DataSourceFactory) extends DataSourceFactory {
  def getDataSource(server: String, database: String, port: Int, user: String, password: String) = {
    val ds = underlying getDataSource(server, database, port, user, password)
    val config = new HikariConfig
    config setDataSource ds
    config setConnectionTestQuery "select 1"
    new HikariDataSource(config)
  }
}

class MSSQLDBC[C](val name: String, filename: String) 
  extends DataSourceDBC[C](filename, new HikariFactory(JtdsFactory))
