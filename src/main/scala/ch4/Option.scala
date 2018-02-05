package ch4

sealed trait Option[+A] {
  def map[B](f: A⇒B): Option[B] =  this match {
    case None ⇒ None
    case Some(x) ⇒ Some(f(x))
  }
  def flatMap[B](f: A⇒Option[B]): Option[B] = this match {
    case None ⇒ None
    case Some(x) ⇒ f(x)
  }
  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case None ⇒  default
    case Some(x) ⇒ x
  }
  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = this match {
    case None ⇒ ob
    case  x ⇒ x
  }
  def filter(f: A ⇒ Boolean): Option[A] =  this match {
    case Some(a) if(f(a)) ⇒ this
    case _ ⇒ None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
object Option {
  def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _ map f
  def map2[A,B,C](a: Option[A], b: Option[B])(f:(A, B) ⇒ C): Option[C] =
    (a,b) match {
      case (None, None) ⇒ None
      case _ ⇒  a.flatMap( x ⇒ b.map( z ⇒ f(x,z) ) )
     }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List()): Option[List[A]])((cur, acc) ⇒
      map2(acc, cur)((x: List[A], y: A) ⇒ y :: x))

  def traverse[A, B](a: List[A])(f: A ⇒ ch4.Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((cur, acc) => map2(f(cur), acc)(_ :: _)  )
}
