package ch5

sealed trait Stream[+A]  {
  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = this match {
    case Cons(h, t) ⇒  f(h(), t().foldRight(z)(f))
    case _ ⇒ z
  }
  def forAll(p: A ⇒ Boolean): Boolean =  this.foldRight(true)(
     (curr, acc) ⇒ p(curr) && acc)
  def headOption: Option[A] = this.foldRight[Option[A]](None) (
    (curr, acc) ⇒
    if(curr != Empty && acc.isEmpty)
      Some(curr)
    else
      None
  )
  def map[B](f: A ⇒ B):Stream[B] = foldRight(Stream.empty[B])(
    (curr, acc) ⇒ Stream.cons(f(curr), acc))

  def filter(f: A ⇒ Boolean) = foldRight(Stream.empty[A]) (
    (curr, acc) ⇒ if(f(curr))
                    Stream.cons(curr, acc)
                  else acc
  )
  def append[B>:A](s: Stream[B]): Stream[B] = foldRight(s)(
    (curr, acc) ⇒ Stream.cons(curr, acc)
  )

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] = foldRight(Stream.empty[B]) (
    (curr, acc) ⇒ f(curr).append(acc)
  )
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()⇒A, t: ()⇒ Stream[A]) extends Stream[A]

object Stream {
  // smart constructor
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] =  {
    lazy val head = hd
    lazy val tail = tl
    Cons[A](() ⇒ head, () ⇒ tl)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = 
  if(as.isEmpty)
    empty[A]
  else
    cons[A](as.head, apply(as.tail: _*) )
  
  def toListRecursive[A](s: Stream[A]): List[A] =  {
    s match {
      case Cons(hd, tl) ⇒ hd() :: toListRecursive[A](tl())
      case _ ⇒ List[A]()
    }
  }
 
    def toList[A](s: Stream[A]): List[A] =  {
      def helper[A](curr: Stream[A], acc: List[A]): List[A] = 
        curr match {
          case Cons(hd, tl) ⇒ hd() :: helper[A](tl(), acc)
          case _ ⇒ acc
        }
      helper(s, List[A]())
    }
  def take[A](n: Int, s: Stream[A]): Stream[A] = {
    def helper[A](_n: Int, acc: Stream[A], curr: Stream[A]): Stream[A] =  
      if( _n < 1 )
        acc
      else  curr match {
        case Cons(h, t) ⇒ helper(_n-1, cons[A](h(), acc), t())
        case _ ⇒ acc
      }
  helper(n, Empty, s)
  }
  def drop[A](n: Int, s: Stream[A]): Stream[A] = {
    def helper[A](_n: Int, curr: Stream[A]): Stream[A] =
      if( _n < 1 )
        curr
      else  curr match {
        case Cons(h, t) ⇒ helper(_n-1, t())
        case _ ⇒ curr
      }
    helper(n, s)
  }

  def takeWhile[A](predicate: A ⇒ Boolean, s: Stream[A]): Stream[A] = {
    def helper(curr: Stream[A], rem: Stream[A]): Stream[A] = rem match {
      case Cons(h, t) if predicate(h()) ⇒
         helper(cons[A](h(), curr), t())
      case _ ⇒ curr
    }
    helper(Empty, s)
  }
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int]= Stream.cons[Int](n, from(n+1))

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] =  {
    def go(state:S, acc: Stream[A]): Stream[A] = f(state) match {
      case Some((a,s)) ⇒ go(s, cons(a, acc))
      case None ⇒ acc
    }
    go(z, empty[A])
  }
}
