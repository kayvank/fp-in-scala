package ch6

import org.specs2._

class RNGSpec extends Specification { def is = s2"""

Chapter 6 Purely Functional state, Random Number Generator
  specification one $e1
  specification tow $e2
  specification three $e3
"""
  def e1 = 1 must_== 0
  def e2 = 1 must_== 0
  def e3 = 1 must_== 0
}
