package ch5

import org.specs2._

class StreamSpec extends Specification { def is = s2"""

chapter 5 Stream Specification

  create a stream $e1
   take(2) from  stream $e2
   takewhile(NotEven) from  stream $e3
   drop(3) from  stream $e4
   foldRight from  stream $e5
   forAll from  stream $e6
   headOpiton from  stream $e7

"""
  val stream: Stream[Int]=Stream.cons(
    1, Stream.cons(
      2, Stream.cons(
        3, Stream.cons(
          4, Stream.cons(
            5, Empty)))))

  def e1 = {
   val s: Stream[Int] = Stream.cons(1+2, Stream.cons((2+1), Empty)) 
    val computed: List[Int] = Stream.toList(s)
    println(s"Stream.toList(s) = ${computed}")
    computed.size must_== 2
  }

  def e2 = {
    val computed = Stream.take[Int](2, stream)
    println(s"Stream.take(2)) = ${Stream.toList(computed)}")
    Stream.toList(computed).size must_== 2
  }

  def e3 = {
    val computed = Stream.takeWhile[Int](x ⇒ x%2 != 0, stream)
    println(s"--Stream.takeWhile(notEven)) = ${Stream.toList(computed)}")
    Stream.toList(computed).size must_== 1
  }

  def e4 = {
    val computed = Stream.drop[Int](3, stream)
    println(s"Stream.drop(3)) = ${Stream.toList(computed)}")
    Stream.toList(computed).size must_== 2
  }

  def e5 = {
    val computed = stream.foldRight[List[String]](Nil)(
      (curr,  acc) ⇒ curr.toString :: acc )
    computed must_== List("1", "2", "3", "4", "5")
  }

  def e6 = {
    val lesThan4s = stream.forAll(x ⇒ x < 4)
    val lesThan6s = stream.forAll(x ⇒ x < 6)
    lesThan6s must_== true
    lesThan6s must_== ! lesThan4s
  }

  def e7 = {
    stream.headOption must_== Some(1)
  }

}
