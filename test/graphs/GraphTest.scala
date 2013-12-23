package graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
class GraphTest extends FlatSpec with Matchers {

  "A Graph" should "allow to add nodes" in {
    val g: UndirectedGraph = Graph.undirected().addNode(Node(1)).addNode(Node(2)).addNode(Node(3))
    assert(g.V.size === 3)
  }

  "A Graph" should "allow to add edges" in {
    val g = Graph.undirected().addNode(Node(1)).addNode(Node(2)).addEdge(Edge(Node(1), Node(2)))
    assert(g.E.size === 1)
  }

  "A undirected graph" should "have adjacent nodes if there is edge between them" in {
    val g = Graph.undirected().addNode(Node(1)).addNode(Node(2)).addEdge(Edge(Node(1), Node(2)))

    assert(g.adj(Node(1)).contains(Node(2)))
    assert(g.adj(Node(1)).size === 1)
    assert(g.adj(Node(2)).contains(Node(1)))
    assert(g.adj(Node(2)).size === 1)
  }

  "A vertices in undirected graph" should "be not be adjacent if there is no edge" in {
    val g = Graph.undirected().addNode(Node(1)).addNode(Node(2)).addNode(Node(3)).addNode(Node(4))

    assert(g.adj(Node(1)).size === 0)
    assert(g.adj(Node(2)).size === 0)
    assert(g.adj(Node(3)).size === 0)
    assert(g.adj(Node(4)).size === 0)
  }

}
