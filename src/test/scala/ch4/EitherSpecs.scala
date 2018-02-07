package ch4

import org.specs2._

class EitherSpec extends Specification { def is = s2"""

Chapter 4 Either specifications
Either 4.6 map $e1
Either 4.6 flatMap $e2
Either 4.6 orElse $e3
Either 4.6 map2 $e4
Either 4.6 sequence $e5
"""

  def e1 = ch4.Right[Int](1).map[ Int](x ⇒ x*2) must_== Right(2)
  def e2 = ch4.Right[Int](1).flatMap(x ⇒ ch4.Right(x*2)) must_== Right(2)
  def e3 = ch4.Left[Int](1).orElse(Right[Char]('1')) must_== Right('1')
  def e4 = {
    val _this: ch4.Either[String, Int]=Right(1)
    val  _that: ch4.Either[String, Double] = Right(1.1)
    def f = (x: Int, y: Double) ⇒ s"${x} & ${y}"
    val actual: Either[String, String] =  Right[String]("1 & 1.1")
    _this.map2[String, Double, String](_that)(f) must_== actual
  }
  def e5 = {
    val  testData: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    val computed : Either[String, List[Int]]= ch4.Either.sequence[String, Int](testData)
    val actual: Either[String, List[Int]] = Right(List(3, 2, 1))
  computed must_==  actual

  }
}
