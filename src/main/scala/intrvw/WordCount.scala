package intrvw

import scala.collection.mutable._

object WordCount {

  case class myTuple(t: (String, Int)) extends Ordered[myTuple] {
    def compare(that: myTuple):Int = {
      val (x,y) =t
      val (x1,y1) = that.t
      if (y.compare(y1) != 0)
        y1.compare(y)
      else x.compare(x1)
    }
  }
  val splitToWords: String ⇒ List[String] =  str ⇒ str.split("\\s+|-").toList

  val countFrequency: List[String] ⇒ List[(String, Int)] = words ⇒
  words.foldLeft( new HashMap[String, Int]()) ((acc, curr) ⇒
    if(acc.exists(_._1 == curr)) {
      acc.update(curr, acc(curr) + 1)
      acc
    } else acc += (curr → 1)
  ).toList

  implicit def tupleToBeordered(t: (String, Int)) = new myTuple(t._1,t._2)

  val sort: List[(String, Int)] ⇒ List[myTuple] = wordCount ⇒
  wordCount.map(x⇒ tupleToBeordered(x)).sorted

  val sort2: List[(String, Int)] ⇒ List[(String, Int)] =
    wordCount ⇒  wordCount
      .toList
      .sortBy(_._2)
      .reverse
      .groupBy(_._2)
      .values.toList.flatMap(_.sortBy(_._1))

  val sort3: List[(String, Int)] ⇒ List[(String, Int)] = wcList ⇒
    wcList.sortBy(_._2)
    .reverse
    .groupBy(_._2)
    .values.toList.flatMap(_.sortBy(_._1))

val asPairs : List[String] ⇒ List[List[(String, Int)]] = words ⇒ 
  words.map((_, 1))
    .groupBy(_._1)
    .values.toList
  val asAggregatedPairs:  List[List[(String, Int)]] ⇒ List[(String, Int)] = pairs ⇒
  pairs.map(x ⇒ (x.head._1,
     x.foldLeft(0)((acc,curr) ⇒ acc + curr._2)) )

  val solution = splitToWords andThen countFrequency
  val  solution1 = solution andThen sort
  val  solution2 = solution andThen sort2
  val  solution3 = splitToWords andThen asPairs andThen asAggregatedPairs andThen sort3
    
}
