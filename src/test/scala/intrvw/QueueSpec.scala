package intrvw

import org.specs2._
import scala.collection.immutable.HashMap
import ch4.{Option ⇒ _}
 class QueueSpec extends Specification { def is = s2"""
enqueue dequeue specifications
  enque must queue correctly $e1
  enque must dequeue correctly $e2
"""
   def e1 = {
     val queue = new Queue[String](5)
     queue.enqueue("one")
     queue.enqueue("two")
     queue.enqueue("three")

     val q1 = queue.dequeue()
     queue.enqueue("")
     queue.dequeue()
     val q2 = queue.dequeue()
     val q3 = queue.dequeue()
     println(s"q3 = ${q3.right}")
     q3 must beRight("three") 
     q2 must beRight("two")
     q1 must beRight("one")
   }
   def e2 =  {
     val queue = new Queue[String](3)
     val testData: List[String] = "one" :: "two" :: "three" :: "for" :: Nil
     val computed = testData map queue.enqueue
     computed.lift(3).get.isLeft
     computed.lift(2).get.isRight
}

}
