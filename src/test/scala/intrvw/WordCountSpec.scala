package intrvw

import org.specs2._
import scala.collection.mutable._

class WordCountSpec extends Specification { def is = s2"""

Chapter 6 Purely Functional state, Random Number Generator
  splitToWords the sentence by white space & - $e1
  frequecny counter method-1 $e2
  frequecny counter method-2 $e3
  frequecny counter method-3 $e4
"""
  import WordCount._ 
  val input = "the quick brown fox jumped over the brown-fox-ed-chair"
  val output = List(
    myTuple(("brown",2)),
    myTuple(("fox",2)),
    myTuple(("the",2)),
    myTuple(("chair",1)),
    myTuple(("ed",1)),
    myTuple(("jumped",1)),
    myTuple(("over",1)),
    myTuple(("quick",1)))

  val output_2 = List(
    ("brown",2),
    ("fox",2),
    ("the",2),
    ("chair",1),
    ("ed",1),
    ("jumped",1),
    ("over",1),
    ("quick",1))
    
  def e1 = WordCount.splitToWords(input).size must_==11

  def e2 = {
    val computed = WordCount.solution1(input)
    computed must_== output
  }
  def e3 = {
    val computed = solution2(input)
    computed must_== output_2 
  }
  def e4 = {
    val computed = solution3(input)
    println(s"---------- computed = ${computed}")

    computed must_== output_2
  }
}
