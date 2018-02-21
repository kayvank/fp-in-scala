package intrvw

import scala.collection._
import scala.collection.convert.decorateAsScala._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic._
import scala.collection.concurrent.Map

case class QE(reason: String)
trait QueueT[T] {
  def enqueue(t: T):Either[QE, Unit]
  def dequeue(): Either[QE, T]
}

class Queue[T](size:Int = 10) extends QueueT[T]{
  var low=new AtomicInteger(0)
  var high=new AtomicInteger(0)
  private final val queue: Map[Int, T] =
    new ConcurrentHashMap().asScala

  def enqueue(t: T): Either[QE, Unit] = {
    if(high.get >=  size)
      return Left(QE(s"max queue-size has reached.hight=${high.get} "))
    queue += (high.get →  t)
    high.set(high.get + 1)
    Right(())
  }

  def dequeue():Either[QE, T] = {
    if(high.get < low.get)
      return Left(QE(s"nothing to dequeu. low=${low.get}  hight=${high.get}"))
    val ret = queue.get(low.get)
    low.set(low.get + 1)
    if(ret.isDefined)
      Right(ret.get)
    else Left(QE(s"map key = ${low.get} returned nothing"))
  }
}
