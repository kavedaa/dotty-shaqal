package org.shaqal

class H2DBC[A](val name: String) extends UrlDBC[A](
  s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1",
  new org.h2.Driver,
  "sa",
  "")