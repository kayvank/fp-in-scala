package ch3.datastructure

import org.specs2._
import ch3.datastructure.List._ 

class DatastructureSpec extends Specification { def is = s2"""

 Chapter 3 specifications
 List sum $e1
 List product $e2
List Ex 3.1 $e3
List Ex 3.2 tail impl $e4
List Ex 3.3.1 setHead $e5
List Ex 3.3.2 drop(list, n) $e6
List Ex 3.3.3 dropWhile(list, ) $e7
"""

  def e1 = sum(List(1,2,3)) must_== 6 // no need to do apply()
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
}