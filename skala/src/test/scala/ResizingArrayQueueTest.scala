import collections.ResizingArrayQueue
import org.scalatest._

class ResizingArrayQueueTest extends FlatSpec with Matchers {

  "A ResizingArrayQueue" should "dequeue values in firs-in-first-out order" in {
    val stack = new ResizingArrayQueue[Int]
    stack.enqueue(56)
    stack.enqueue(66)
    stack.dequeue() should be(56)
    stack.dequeue() should be(66)
  }


  it should "throw NoSuchElementException if an empty queue is dequeued" in {
    val emptyQueue = new ResizingArrayQueue[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyQueue.dequeue()
    }
  }

  it should "implement iterator to iterate over values in first-in-first-out order" in {
    val stack = new ResizingArrayQueue[Int]
    stack.enqueue(56)
    stack.enqueue(66)

    val it = stack.iterator
    var i = 0
    while (it.hasNext) {
      println(it.next())
      i += 1
    }

    i should be(2)
  }

}