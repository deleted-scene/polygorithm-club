import scala.reflect.ClassTag

case class ResizingArrayStack[T : ClassTag](){

  private var array: Array[T] = Array.ofDim[T](2)
  private var N:Int          = 0

  def isEmpty(): Boolean = N ==  0
  def size (): Integer   = N

  def resize(capacity: Int) = {
    assert(capacity > N)

    val temp = Array.ofDim[T](capacity)
    for(i:Int <- array.indices ) temp(i) = array(i)
    array = temp
  }

  def push(item: T): Unit ={
    if (array.length == N) resize( 2 * array.length)

    array(N) = item
    N += 1
  }

  def pop(): T ={
    if (isEmpty()) throw new NoSuchElementException("Stack is empty.")

    val item = array(N-1)

    array(N-1) = _:T //to avoid loitering

    N -= 1
    //resize if necessary
    if(N > 0  && N == array.length /4 ) resize(array.length /4 )

    item
  }

}
