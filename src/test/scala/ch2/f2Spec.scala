import org.specs2._
import ch2.F2Impl._
class F2Spec extends Specification { def is = s2"""

Chapter 2 specification
  simple HOF $e1
  curry function $e2
  function compositions $e3
"""
  def e1 = f2FImpl(5, 6, ((x,y) ⇒ x<y)) must_== true
  def e2 = {
    def f(i: Int, d: Double) : String = s"${i.toString} , ${d.toString}"
    val g = curry(f)
    g(1)(1.3) must_== f(1, 1.3)
  }
  def e3 = {
    def f(s: String): String = s"appending to string: ${s}"
    def g(i: Int): String = s"${i}"
    val fg = compose(f, g)
    fg(2) must_== f(g(2))

  }
}
