package collections

import scala.reflect.ClassTag

case class ResizingArrayQueue[T: ClassTag]() extends Iterable[T] {

  private var _array: Array[T] = Array.ofDim[T](2)
  private var N: Int = 0
  private var _first: Int = 0
  private var _last: Int = 0

  override def size(): Int = N


  def enqueue(item: T): Unit = {

    if (N == _array.length) _resize(2 * _array.length)

    _array(_last) = item
    _last += 1

    if (_last == _array.length) _last = 0
    N += 1

  }

  def dequeue(): T = {

    if (isEmpty()) throw new NoSuchElementException("Queue is empty.")

    val item: T = _array(_first)

    _array(_first) = _: T // To avoid loitering

    N -= 1
    _first += 1
    if (_first == _array.length) _first = 0


    if (N > 0 && N == _array.length / 4) _resize(_array.length / 2)

    item
  }

  private def _resize(capacity: Int) = {
    assert(capacity >= N)

    val temp = Array.ofDim[T](capacity)

    for (i <- 0 until N) {
      temp(i) = _array((_first + i) % _array.length)
    }

    _array = temp
    _first = 0
    _last = N
  }

  override def isEmpty(): Boolean = N == 0

  override def iterator: Iterator[T] = new ResizableArrayQueueIterator()

  private class ResizableArrayQueueIterator extends Iterator[T] {
    private var i = 0

    override def next(): T = {
      if (!hasNext) throw new NoSuchElementException()

      val item: T = _array((i + _first) % _array.length)
      i += 1
      item
    }

    override def hasNext: Boolean = i < N
  }

}
