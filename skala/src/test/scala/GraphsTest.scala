import graphs.graphs.AdjMatrixGraph
import org.scalatest._

class GraphsTest extends FlatSpec with Matchers {

  "A AdjMatrixGraph" should " print stuff" in {

    val g = new AdjMatrixGraph(20)

    g.addVertex("Finike")
    g.addVertex("Kumluca")
    g.addVertex("Antalya")
    g.addVertex("Elmali")

    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(2, 3)

    g.print()

    true should be(true)
  }
}