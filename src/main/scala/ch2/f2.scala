package ch2

trait F2[A, B, C] {
def apply(a: A, _a: A, b: B ): C
}
object F2Impl {
  val f2FImpl = new F2[Int, (Int, Int) ⇒ Boolean, Boolean] {
    def apply(a: Int, b: Int, f: (Int, Int) ⇒ Boolean): Boolean =  f(a, b)
  }
  def partiall[A,B,C](a: A, f: (A,B) ⇒ C) : B ⇒ C =
    b ⇒ f(a, b)
  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ (B ⇒ C) =  (a: A) ⇒ (b: B) ⇒ f(a,b)
  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C = (a: A,b: B) ⇒  f(a)(b)
  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C = (a: A) ⇒ f(g(a))

}
