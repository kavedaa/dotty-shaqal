trait Foo

trait Foo1 extends Foo
object Foo1 extends Foo1

trait Foo2 extends Foo
object Foo2 extends Foo2

class Bar[A]

given Bar[Foo1] = new Bar[Foo1]
given Bar[Foo2] = new Bar[Foo2]

extension foo on [F1 <: Foo, F2 <: Foo](t: (F1, F2)) {

  def test(g: (Bar[F1], Bar[F2]) ?=> Int)(using bar1: Bar[F1], bar2: Bar[F2]) = {
    g(using bar1, bar2)
  }
}


(Foo1, Foo2).test {
  val h1 = summon[Bar[Foo1]]
  val h2 = summon[Bar[Foo2]]
  1
}
