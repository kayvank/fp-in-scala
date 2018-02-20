package ch6

import org.specs2._
import scala.collection.immutable._

class RNGSpec extends Specification { def is = s2"""

Chapter 6 Purely Functional state, Random Number Generator
  specification one $e1
  specification tow $e2
  specification three $e3
"""
  val input = " the quick brown fox jumped over the brown-foxed-chair"
  def splitter(str: String): List[String] =  str.split("\\s+|-").toList
  def freqCounter(words: List[String]): Map[String, Int] = {
    new HashMap[String, Int]()
  }
    
  def e1 = 1 must_== 0
  def e2 = 1 must_== 0
  def e3 = 1 must_== 0
}
