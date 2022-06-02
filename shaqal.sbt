name := "shaqal"

organization := "org.shaqal"

version := "2.0-SNAPSHOT"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.170" % "test",
	"net.sourceforge.jtds" % "jtds" % "1.3.1" % "test",
	"org.postgresql" % "postgresql" % "9.4.1212" % "test",
	"com.zaxxer" % "HikariCP" % "2.6.1" % "test"
)
