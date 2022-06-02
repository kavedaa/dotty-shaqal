package org.shaqal

import java.sql._

import java.util.Properties
import java.io._
import javax.sql.DataSource

abstract class DBC[C] 
  extends ConnectorBase[C]:
  def close(conn: Connection) = conn.close()

abstract class UrlDBC[C](
  url: String,
  driver: Driver,
  username: String,
  password: String)
  extends DBC[C]:

  def this(protocol: String, properties: Properties, driver: Driver) =
    this(
      s"$protocol://${properties getProperty "server"}:${(properties getProperty "port").toInt}/${properties getProperty "database"}",
      driver,
      properties getProperty "username",
      properties getProperty "password")

  def this(protocol: String, propertiesFileName: String, driver: Driver) =
    this(
      protocol,
      {
        val properties = new Properties
        properties load new FileInputStream(propertiesFileName)
        properties
      },
      driver)

  DriverManager registerDriver driver

  def getConnection() = DriverManager getConnection (url, username, password)

abstract class DataSourceDBC[C](
  server: String,
  dbName: String,
  port: Int,
  username: String,
  password: String,
  dsFactory: DataSourceFactory)
  extends DBC[C]:

  def this(properties: Properties, dsFactory: DataSourceFactory) =
    this(
      properties getProperty "server",
      properties getProperty "database",
      (properties getProperty "port").toInt,
      properties getProperty "username",
      properties getProperty "password",
      dsFactory)

  def this(propertiesStream: InputStream, dsFactory: DataSourceFactory) =
    this(
      {
        val properties = new Properties
        properties load propertiesStream
        properties
      },
      dsFactory)

  def this(propertiesFile: File, dsFactory: DataSourceFactory) =
    this(
      new FileInputStream(propertiesFile),
      dsFactory)

  def this(propertiesFileName: String, dsFactory: DataSourceFactory) =
    this(
      new File(propertiesFileName),
      dsFactory)

  val dataSource = dsFactory getDataSource (server, dbName, port, username, password)

  def getConnection() = dataSource.getConnection()

abstract class DataSourceFactory:
  def getDataSource(server: String, database: String, port: Int, user: String, password: String): DataSource