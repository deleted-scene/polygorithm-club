import graphs.graphs.{AdjListGraph, AdjMatrixGraph}
import org.scalatest._

class GraphsTest extends FlatSpec with Matchers {


  "A AdjListGraph" should " print stuff" in {

    val g = new AdjListGraph[String]()

    g.addNode("Finike")
    g.addNode("Kumluca")
    g.addNode("Antalya")
    g.addNode("Elmali")

    g.addUndirectedEdge("Finike", "Kumluca", 20)
    g.addUndirectedEdge("Kumluca", "Antalya", 160)
    g.addUndirectedEdge("Antalya", "Elmali", 140)
    g.addUndirectedEdge("Finike", "Elmali", 60)

    g.print()

    true should be(true)
  }

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