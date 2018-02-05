package ch4

import org.specs2._

class OptionSpec extends Specification { def is = s2"""

chapter 4 Opiton specification
 Option Ex 4.1 map $e1
 Option Ex4.1 flatmap $e2
 Opiotn Ex 4.1 getOrElse $e3
 Opiotn Ex 4.1 orElse $e4
 Opiotn Ex 4.1 filter $e5
 Opiotn Ex 4.2 lift $e6
 Opiotn Ex 4.3 map2 $e7
 Opiotn Ex 4.4 sequence $e8
 Opiotn Ex 4.5 traverse $e9
"""
  def e1 = ch4.Some(10).map(x ⇒ x*2) must_== ch4.Some(20)
  def e2 = ch4.Some(10).flatMap(x ⇒ Some(x*2)) must_== ch4.Some(20)
  def e3 = ch4.None.getOrElse(20) must_== 20
  def e4 = ch4.None.orElse(Some(20)) must_== ch4.Some(20)
  def e5 = ch4.Some(20).filter(x ⇒ x > 20 ) must_== None
  def e6 = ch4.Some(-49) lift math.abs  must_== ch4.Some(math.abs(-49))
  def e7 = ch4.Option.map2[Int, Double, String](ch4.Some(1), ch4.Some(1.1))(
    (x:Int, y:Double) ⇒ s"${x} --- ${y}") must_== ch4.Some(s"1 --- 1.1")
  def e8 = ch4.Option.sequence[Int](
    List(ch4.Some(1), ch4.Some(2))) must_== ch4.Some(List(1,2))
  def e9 = {
def f(a: Int ): ch4.Option[Int] =  ch4.Some(a)
  ch4.Option.traverse[Int, Int](List(1,2, 3))(f) must_== ch4.Some(List(1,2,3))
  }
}
