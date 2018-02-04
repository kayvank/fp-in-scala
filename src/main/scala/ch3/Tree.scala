package ch3.datastructure

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 1
    case Branch(l,r) ⇒ size(l) + size(r)
  }
  def maxNum(i: Int)(j: Int): Int = if(i > j) i; else j
  def maximum(t: Tree[Int]): Int =  {
    def helper (max: Int, t: Tree[Int]): Int = t match {
      case Leaf(v) ⇒  if (v > max ) v; else max
      case Branch(l, r) ⇒
        maxNum( helper(max, l))(helper(max, r))
    }
    helper(0, t)
  }
  def depth[A](t: Tree[A]): Int =  {
    def helper(d: Int, t: Tree[A]): Int = t match {
      case Leaf(_) ⇒ d+1
      case Branch(l, r) ⇒
        maxNum(helper(d+1, l))(helper(d+1, r) )
    }
    helper(0, t)
  }
  def map[A](f: A ⇒ A, t: Tree[A]): Tree[A] = ???
}
 
