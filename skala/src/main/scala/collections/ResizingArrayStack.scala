import scala.reflect.ClassTag

case class ResizingArrayStack[T: ClassTag]() {

  private var array: Array[T] = Array.ofDim[T](2)
  private var N: Int = 0

  def size(): Integer = N

  def push(item: T): Unit = {
    if (array.length == N) _resize(2 * array.length)

    array(N) = item
    N += 1
  }

  private def _resize(capacity: Int) = {
    assert(capacity > N)

    val temp = Array.ofDim[T](capacity)
    for (i: Int <- array.indices) temp(i) = array(i)
    array = temp
  }

  def pop(): T = {
    if (isEmpty()) throw new NoSuchElementException("Stack is empty.")

    val item = array(N - 1)

    array(N - 1) = _: T //to avoid loitering

    N -= 1
    //resize if necessary
    if (N > 0 && N == array.length / 4) _resize(array.length / 4)

    item
  }

  def isEmpty(): Boolean = N == 0

}
