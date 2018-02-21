package ch6

import org.specs2._
import scala.collection.mutable._

class WordCountSpec extends Specification { def is = s2"""

Chapter 6 Purely Functional state, Random Number Generator
  splitToWords the sentence by white space & - $e1
  frequecny counter $e2
"""
  case class myTuple(t: (String, Int)) extends Ordered[myTuple] {
    def compare(that: myTuple):Int = {
      val (x,y) =t
      val (x1,y1) = that.t
      if (y.compare(y1) != 0)
        y1.compare(y)
      else x.compare(x1)
    }
  }

  val input = "the quick brown fox jumped over the brown-fox-ed-chair"
  val output = Vector(
    myTuple(("brown",2)),
    myTuple(("fox",2)),
    myTuple(("the",2)),
    myTuple(("chair",1)),
    myTuple(("ed",1)),
    myTuple(("jumped",1)),
    myTuple(("over",1)),
    myTuple(("quick",1)))
  val splitToWords: String ⇒ List[String] =  str ⇒ str.split("\\s+|-").toList
  val countFrequency: List[String] ⇒ Map[String, Int] = words ⇒ 
    words.foldLeft( new HashMap[String, Int]()) (
      (acc, curr) ⇒
      if(acc.exists(_._1 == curr)) {
        acc.update(curr, acc(curr) + 1)
        acc
      }
      else acc += (curr → 1)
    )

  implicit def tupleToBeordered(t: (String, Int)) = new myTuple(t._1,t._2)
  val sort: Map[String, Int] ⇒ Vector[myTuple] = wordCount ⇒ wordCount.toVector.map(x⇒ tupleToBeordered(x)).sorted
    val __sort: Map[String, Int] ⇒ Vector[(String, Int)] = wordCount ⇒ wordCount.toVector.sortWith(
    (l,r) ⇒ (l._2.toString + l._1.toString) > (r._2.toString + r._1.toString))
  
  val solution1 = splitToWords andThen countFrequency andThen sort
    
  def e1 = splitToWords(input).size must_==11

  def e2 = {
    val computed = solution1(input)
    computed must_== output
  }
}
