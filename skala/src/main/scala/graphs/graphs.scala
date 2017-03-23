package graphs


object graphs {

  //simple implementations

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
