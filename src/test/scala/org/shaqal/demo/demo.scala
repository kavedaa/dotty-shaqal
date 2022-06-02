package org.shaqal.demo

import org.shaqal.*
import org.shaqal.table.*
import org.shaqal.column.*

import scala.deriving.Mirror

trait PersonTable extends TableBase:

  val firstName = new varchar(max)("firstName") with notnull
  val lastName = new varchar(128)("lastName") with notnull
  val age = new int("age") with notnull

  val foo = "bar"

  val * = (firstName, lastName, age)

  type Relation = *.type
  def relation = *  

object DemoDB:

  object Person extends Table("Person") with PersonTable:
    val a = summon[RelationTypes =:= (String, String, Int)]
    val m = summon[Mirror.ProductOf[(String, String, Int)]]
    val ev = summon[m.MirroredElemTypes =:= RelationTypes]
    val ev2 = summon[Tuple.IsMappedBy[W][Relation]]

  val a = Person.select(_.foo)
  val b = Person.select(_.age)
  val c = Person.select(x => (x.firstName, x.age))
  val d = Person.select(x => ((x.firstName, x.lastName), x.age))
  val e = Person.select(_.*)

  Person.insert(("Tom", "Smith", 29))

//  val aa = a.read
  val bb = b.read
//  val cc = c.read
