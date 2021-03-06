package ch3.datastructure

import org.specs2._
import ch3.datastructure.List._

class ListSpec extends Specification { def is = s2"""

 Chapter 3 List specifications
  List sum $e1
  List product $e2
  List Ex 3.1 $e3
  List Ex 3.2 tail impl $e4
  List Ex 3.3.1 setHead $e5
  List Ex 3.3.2 drop(list, n) $e6
  List Ex 3.3.3 dropWhile $e7
  List Ex 3.3.6 init list $e8
  List Ex 3.3.7 foldRigth list $e9
  List Ex 3.3.9 compute length of list using foldRigth  $e10
  List Ex 3.3.11 Sum foldLeft impl  $e12
  List Ex 3.3.11 length foldLeft impl  $e13
  List Ex 3.3.11 product foldLeft impl  $e14
  List Ex 3.3.18 map foldLeft impl  $e15
  List Ex 3.3.19 filter foldLeft impl  $e16
  List Ex 3.3.20 flatAppend foldLeft impl  $e17
  List Ex 3.3.20 flatMap foldLeft impl  $e18
"""
  def e1 = sum(List(1,2,3)) must_== 6 
  def e2 = product(List(2.0, 3.0, 4.0)) must_== 24.0
  def e3 = {
    val x: Int = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) ⇒ x
      case Nil ⇒ 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
      case Cons(h, t) ⇒ h + sum(t)
      case _ ⇒ 101
    }
    x must_== 3
  }
  def e4 = List(1,2,3,4,5) must_== tail(List(0,1,2,3,4,5))
  def e5 = List(1,2,3) must_== setHead(List(-1,2,3), 1)
  def e6 = List(1,2,3) must_== drop(List(-1,0, 1, 2,3), 2)
  def e7 = List(2,3,4,5) must_== dropWhile(List(-1, 0, 1, 2, 3, 4, 5), (x: Int) ⇒ x < 2) 
  def e8 = List(2,3,4,5) must_== init(List(2, 3, 4, 5, 6 ))
    // (x,y) ⇒ x+y scala can infer types of x & y
  def e9 = _foldRight(List(1,2,3, 4), 0)( _+_) must_== 10
  def e10 = foldRight(List(1,2,3,4), 0)((curr, acc) ⇒  acc + 1) must_== 4
  def e11 = foldLeft(List(1,2,3,4), 0)((acc,curr) ⇒ acc + curr) must_== 10
  def e12 = foldLeftSum(List(1,2,3)) must_== 6
  def e13 = foldLeftLength(List(1,2,3)) must_== 3
  def e14 = foldLeftProduct(List(1,2,3,4)) must_== 24 
  def e15 = foldMap[Int, Int](List(1,2,3,4))((x ⇒ x*3): Int ⇒ Int
  ) must_== reverse(List(3, 6, 9, 12))
  def e16 = foldFilter[Int](List(0,1,2,3,4))((x ⇒ x<3): Int ⇒ Boolean ) must_== reverse(List(0,1,2))
  def e17=foldAppend[Int](List(5, 4), List(1,2,3)) must_== List(4, 5, 1,2,3)
  def e18 = foldFlatMap[Int, Int](List(1,2,3))(x ⇒ List(x, x))  must_==
    reverse(List(1,1,2,2,3,3))

}
