package graphs

import Graph._

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
trait GraphsFixtures {

  def emptyGraph(n: Int): UndirectedGraph = {
    var g = Graph.undirected()
    for (i <- 0 to n) {
      g = g.addNode(Node(i))
    }
    g
  }

  def completeGraph(n: Int, offset: Int): UndirectedGraph = {
    val nodes: Set[Node] = {
      for (i <- 1 to n) yield Node(i + offset)
    }.toSet

    val edges: Set[Edge] = {
      for {
        i <- 1 to n
        j <- 1 to n
        if i != j
      } yield Edge(Node(i + offset), Node(j + offset))
    }.toSet
    new UndirectedGraph(nodes, edges)
  }

  def completeGraph(n: Int): UndirectedGraph = {
    completeGraph(n, 0)
  }

  def disconnectedGraphWithTwoCompleteGraphs(n1: Int, n2: Int) = {

    val g1: UndirectedGraph = completeGraph(n1)
    val g2: UndirectedGraph = completeGraph(n2, n1)

    new UndirectedGraph(g1.V ++ g2.V, g1.E ++ g2.E)
  }

  def cycle(n: Int): UndirectedGraph = {
    require(n >= 3)

    val nodes: Set[Node] = {
      for (i <- 1 to n) yield Node(i)
    }.toSet

    val edges: Set[Edge] = {
      for {
        i <- 1 to n
        j <- 1 to n
        if i + 1 <= n
      } yield Edge(Node(i), Node(i + 1))
    }.toSet
    new UndirectedGraph(nodes, edges + Edge(Node(1), Node(n)))
  }

  def circle(n: Int): UndirectedGraph = {
    require(n >= 4)

    val g: UndirectedGraph = cycle(n - 1)
    val additionalEdges: Set[Edge] = {
      for (i <- 1 until n) yield Edge(Node(i), Node(n))
    }.toSet

    new UndirectedGraph(g.V + Node(n), g.E ++ additionalEdges)
  }

  /**
   * Simple binary tree with 5 nodes
   * 1
   * / \
   * 2  3
   * / \
   * 4   5
   */
  def tree: UndirectedGraph = {
    Graph.undirected().addNode(1).addNode(2).addNode(3)
      .addNode(4).addNode(5).addEdge((1, 2)).addEdge((1, 3))
      .addEdge((2, 4)).addEdge((2, 5))
  }
}
