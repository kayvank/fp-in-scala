package ch3.datastructure

sealed trait List[+A] // List data type is polymorphic in the type of parameters it contains
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  
  def apply[A](as: A*): List[A] =// variadic func accepts 0 or more args
    if(as.isEmpty) Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def _sum(ints: List[Int]): Int =  ints match {
    case Nil ⇒ 0
    case Cons(h, t) ⇒ h + _sum(t)
   }
  def sum(ints: List[Int]): Int = {
    def helper(acc: Int, a:List[Int]): Int =  a match {
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

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil ⇒ Nil
    case Cons(_,  Nil) ⇒  Nil
    case Cons(h,  t) ⇒  Cons(h, init(t))
  }

  def _foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B):  B = as match {
    case Nil ⇒ z
    case Cons(h, t) ⇒ f(h, foldRight(t, z)(f) )
  }
  
// @tailrec
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B):  B =  {
    def helper(_z: B, ls: List[A]): B = {
      ls match {
        case Nil ⇒ _z
        case Cons(h, t) ⇒  helper(f(h, _z), t)
      }
    }
    helper(z, as)
  }
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B):  B =  {
    def helper(_z: B, ls: List[A]): B = {
      ls match {
        case Nil ⇒ _z
        case Cons(h, t) ⇒  helper(f(_z, h), t)
      }
    }
    helper(z, as)
  }

  def foldSum[A](z: Int, ls: List[A], f: (Int, A) ⇒ Int): Int =
    foldLeft[A, Int](ls, z)(f)

  def foldLeftSum(ls: List[Int] ) =

    foldSum[Int](0, ls,  _ + _)

  def foldLeftProduct(ls: List[Int] ) =
    foldSum[Int](1, ls,  _ * _)

  def foldLeftLength[A](ls: List[A] ) =
    foldSum[A](0, ls, (acc: Int, curr: A) ⇒ acc+ 1)

  def reverse[A] (ls: List[A]): List[A] =
    foldLeft[A, List[A]](ls, Nil)( (acc, cur) ⇒ Cons(cur, acc))

  def foldMap[A, B](as: List[A])(f: A⇒B): List[B] =
    foldLeft[A, List[B]](as, Nil)((acc, cur) ⇒ Cons(f(cur), acc ) )

  def foldFilter[A](as: List[A])(p: A ⇒ Boolean): List[A] = 
    foldLeft[A, List[A]](as, Nil)((acc, cur) ⇒ if(p(cur)) Cons(cur, acc); else acc )

  def  foldAppend[A](as: List[A], bs: List[A]): List[A] = 
    foldLeft[A, List[A]](as, bs)( (acc, cur) ⇒ Cons(cur, acc) ) 
   
  def foldFlatMap[A, B](as: List[A])(f: A ⇒ List[B]): List[B] = 
    foldLeft[A, List[B]](as, Nil)( (acc, cur) ⇒ foldAppend(f(cur),acc ) ) 
}
