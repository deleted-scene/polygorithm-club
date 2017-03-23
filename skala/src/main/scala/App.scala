import graphs.graphs.AdjMatrixGraph

object App {


  def main(args: Array[String]): Unit = {

    val g = new AdjMatrixGraph(20)

    g.addVertex("Finike")
    g.addVertex("Kumluca")
    g.addVertex("Antalya")
    g.addVertex("Elmali")

    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(2, 3)
    g.addEdge(0, 3)

    g.print()
  }
}