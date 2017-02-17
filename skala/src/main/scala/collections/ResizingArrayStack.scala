package collections

import scala.reflect.ClassTag

case class ResizingArrayStack[T: ClassTag]() extends Iterable[T] {

  private var array: Array[T] = Array.ofDim[T](2)
  private var N: Int = 0

  override def size(): Int = N

  def push(item: T): Unit = {
    if (array.length == N) _resize(2 * array.length)

    array(N) = item
    N += 1
  }

  def pop(): T = {
    if (isEmpty()) throw new NoSuchElementException("Stack is empty.")

    val item = array(N - 1)

    array(N - 1) = _: T //to avoid loitering

    N -= 1
    //resize if necessary
    if (N > 0 && N == array.length / 4) _resize(array.length / 2)

    item
  }

  private def _resize(capacity: Int) = {
    assert(capacity > N)

    val temp = Array.ofDim[T](capacity)
    for (i: Int <- array.indices) temp(i) = array(i)
    array = temp
  }

  override def isEmpty(): Boolean = N == 0

  override def iterator: Iterator[T] = new ResizingArrayStackIterator()

  private class ResizingArrayStackIterator extends Iterator[T] {
    private var i = N

    override def next(): T = {
      if (!hasNext) throw new NoSuchElementException()

      val item = array(i - 1)
      i -= 1
      item
    }

    override def hasNext: Boolean = i > 0
  }

}
