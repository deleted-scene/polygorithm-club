import org.scalatest._

class ResizingArrayStackTest extends FlatSpec with Matchers {

  "A ResizingArrayStack" should "pop values in last-in-first-out order" in {
    val stack = new ResizingArrayStack[Int]
    stack.push(56)
    stack.push(66)
    stack.pop() should be(66)
    stack.pop() should be(56)
  }


  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new ResizingArrayStack[Int]
    a[NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }

}