package ch4

import scala.{Either ⇒  _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A ⇒ B): Either[E, B]  =  this match {
    case l: Left[E] ⇒  l
    case Right(r)  ⇒ Right[B](f(r))
  }
  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B]  = this match {
    case l: Left[E] ⇒ l
    case Right(r) ⇒ f(r)
  }
  def orElse[EE >: E, B](b: ⇒ Either[EE, B]): Either[EE, B] = this  match {
    case l: Left[E] ⇒ b
    case r: Right[B] ⇒ r
  }
  def map2[EE>: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = for {
    _a ← this
    _b ← b
  }yield f(_a, _b)


}
case class Left[+E] (get: E) extends Either[E, Nothing]
case class Right[+A] (get: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldLeft(Right(List()): Either[E, List[A]])( (acc, curr) ⇒ curr match {
                                                     case Right(r) ⇒ acc.map(x ⇒ r :: x)
                                                     case l: Left[E] ⇒   l
                                                   })
  def traverse[E, A, B](es: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] = ???
}
