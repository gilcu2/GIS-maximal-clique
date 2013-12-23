package graphs

/**
 * @author Marek Lewandowski <marek.lewandowski@icompass.pl>
 * @since 12/23/13
 */
trait EmptyGraphFixture {

  def emptyGraph(n: Int): UndirectedGraph = {
    var g = Graph.undirected()
    for(i <- 0 to n) {
       g = g.addNode(Node(i))
    }
    g
  }
}
