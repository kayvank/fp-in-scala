import org.specs2._
import ch2.F2Impl._
class F2Spec extends Specification { def is = s2"""

This is my specification
  where chapter 2 apply example is an HOF $e1
  where chapter 2 curry method is an HOF $e2
"""
  def e1 = f2FImpl(5, 6, ((x,y) ⇒ x<y)) must_== true
  def e2 = {
    def f(i: Int, d: Double) : String = s"${i.toString} , ${d.toString}"
    val g = curry(f)
    g(1)(1.3) must_== f(1, 1.3)
  }
}
