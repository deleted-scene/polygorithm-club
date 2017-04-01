package graphs


object graphs {

  class Node[T](val value: T, var neighbours: List[Node[T]] = List[Node[T]]()) {
    var costs: List[Int] = List[Int]()
  }

  class AdjListGraph[T](var nodes: List[Node[T]] = List[Node[T]]()) {


    def addNode(node: Node[T]): Unit = nodes = node :: nodes

    def addNode(value: T): Unit = addNode(new Node[T](value))

    def addDirectedEdge(from: Node[T], to: Node[T], cost: Int): Unit = {
      from.neighbours = to :: from.neighbours
      from.costs = cost :: from.costs
    }

    def addDirectedEdge(from: T, to: T, cost: Int): Unit = {

      val fromNode: Option[Node[T]] = nodes.find(p => p.value.equals(from))
      val toNode: Option[Node[T]] = nodes.find(p => p.value.equals(to))

      if (fromNode.isDefined && toNode.isDefined) {
        fromNode.get.neighbours = toNode.get :: fromNode.get.neighbours
        fromNode.get.costs = cost :: fromNode.get.costs
      }
    }

    def addUndirectedEdge(from: Node[T], to: Node[T], cost: Int): Unit = {
      addDirectedEdge(from, to, cost)
      addDirectedEdge(to, from, cost)
    }

    def addUndirectedEdge(from: T, to: T, cost: Int): Unit = {
      addDirectedEdge(from, to, cost)
      addDirectedEdge(to, from, cost)
    }

    def contains(value: T): Boolean = nodes.exists(p => p.value.equals(value))

    def count(): Int = nodes.size

    def remove(value: T): Boolean = {

      val node: Option[Node[T]] = nodes.find(p => p.value.equals(value))

      node match {
        case None => false
        case Some(`node`) => {
          nodes = nodes diff List(node)


          for (e <- nodes) {

            val index: Int = e.neighbours.indexOf(node.get)

            if (index != -1) {
              e.neighbours = e.neighbours.patch(index, Nil, 1)
              e.costs = e.costs.patch(index, Nil, 1)
            }
          }

          true
        }
      }
    }

    def print(): Unit = {
      for (node <- nodes) {
        for (neighbour <- node.neighbours) {
          println("%s -> %s" format(node.value, neighbour.value))
        }
      }
    }
  }


  //simple adj matrix implementation
  class Vertex(val label: String, isVisited: Boolean = false)

  class AdjMatrixGraph(private val numOfVertices: Int) {
    private val adjMatrix: Array[Array[Int]] = Array.fill[Int](numOfVertices, numOfVertices)(0)
    private val vertices: Array[Vertex] = new Array[Vertex](numOfVertices)
    var indicesCount: Int = 0

    def addVertex(label: String): Unit = {
      vertices(indicesCount) = new Vertex(label)
      indicesCount = indicesCount + 1
    }

    def addEdge(from: Int, to: Int): Unit = {
      adjMatrix(from)(to) = 1
      adjMatrix(to)(from) = 1
    }

    def print(): Unit = {
      for (i <- 0 until indicesCount) {
        for (j <- 0 until indicesCount) {
          if (adjMatrix(i)(j) == 1) {
            println(String.format("%s -> %s", vertices(i).label, vertices(j).label))
          }
        }
      }
    }
  }

}
