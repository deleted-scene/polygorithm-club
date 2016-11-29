package collections


case class ResizingArrayStack[T](){

  private var array: Array[Object] = new Array[Object](2)
  private var N:Int          = 0

  def isEmpty(): Boolean = N ==  0
  def size (): Integer   = N

  def resize(capacity: Int) = {
    assert(capacity > N)

    val temp = new Array[Object](capacity)
    //for(i:Int <- array.indices ) temp(i) = array(i)
    System.arraycopy(array,0, temp,0,capacity)
    array = temp
  }

  def push(item: T): Unit ={
    if (array.length == N) resize( 2 * array.length)
    N = N + 1
    array(N) = item.asInstanceOf[Object]
  }

  def pop(): T ={
    if (isEmpty()) throw new NoSuchElementException("Stack is empty.")

    val item = array(N - 1)
    array(N-1) = null
    N = N -1

    //resize if necessary
    if(N > 0  && N == array.length /4 ) resize(array.length /4 )

    item.asInstanceOf[T]
  }

}