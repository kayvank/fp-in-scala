package ch3.datastructure

sealed trait List[+A] // List data type is polymorphic in the type of parameters it contains
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def _sum(ints: List[Int]): Int =  ints match {
    case Nil ⇒ 0
    case Cons(h, t) ⇒ h + _sum(t)
  }
  def sum(ints: List[Int]): Int = {
    def helper(acc: Int, a:List[Int]): Int = a match {
      case Nil ⇒  acc
      case Cons(h, t) ⇒ helper(acc + h, t)
    }
    helper(0, ints)
  }

  def _product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(h, t) ⇒ h * _product(t)
  }

  def product(ds: List[Double]): Double = {
    def helper(p: Double, d: List[Double]): Double = d match {
      case Nil ⇒ p
      case Cons (0.0, _) ⇒  0.0
      case Cons(h, t) ⇒ helper(p*h, t)
    }
    helper(1.0, ds)
  }

  def tail[A](l: List[A]) = l match {
    case  Nil ⇒ Nil //this is not a good choice. should return Option
    case Cons(h, Nil) ⇒ Nil
    case Cons(h, t) ⇒ t
  }

  def dropWhile[A](ls: List[A], f: A ⇒ Boolean) : List[A] =  ls match {
    case Nil ⇒ Nil
    case Cons(h, Nil) ⇒ if(f(h)) Nil else List(h)
    case Cons(h, t) ⇒ if(f(h)) dropWhile(t, f) else Cons(h, t)
  }
     
  def drop[A](ls: List[A], n: Int): List[A]=  ls match  {
    case Nil ⇒ Nil
    case Cons(h, Nil) ⇒ if(n == 0) List(h) else Nil 
    case Cons(h, t) ⇒ if(n > 1) drop(t, n-1) else  t
  }
  
  def setHead[A](ls: List[A], a:A) : List[A] =  ls match {
    case Nil ⇒ Nil
    case Cons(_, t) ⇒ Cons(a, t)
  }

  // def init[A](ls: List[A]): List[A] = ls match {
  //   case Nil ⇒ Nil
  //   case Cons(h,  t) ⇒ 
  // }
  // example of variadic function, accepts 0 or more args
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else
      Cons(as.head, apply(as.tail: _*))

}
