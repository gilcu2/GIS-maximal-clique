package graphs

import scala.language.postfixOps
import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import Graph._
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
class GraphTest extends FlatSpec with Matchers with GraphsFixtures {

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

  "A empty graph" should "have maximal clique of size 1" in {
    for (n <- 1 to 10) {
      val g = emptyGraph(n)
      assert(Graph.maximalClique(g).size == 1)
      assert(Graph.bronKerbosch(g).size == 1)
    }
  }

  "A complete graph" should "have maximal clique of size equal to the number of nodes" in {
    for (n <- 1 to 30) {
      val g = completeGraph(n)
      assert(Graph.maximalClique(g).size == n)
      assert(Graph.bronKerbosch(g).size == n)
    }
  }

  "A disconnected graph with two complete graphs" should "have maximal clique of size equal " +
    "to the number of nodes in bigger complete graph" in {
    val g = disconnectedGraphWithTwoCompleteGraphs(10, 30)
    assert(Graph.maximalClique(g).size == 30)
    assert(Graph.bronKerbosch(g).size == 30)
  }

  "A cycle graph" should "have maximal clique of size 1" in {
    for (i <- 4 to 10) {
      assert(Graph.maximalClique(cycle(i)).size === 2)
      assert(Graph.bronKerbosch(cycle(i)).size === 2)
    }
  }

  "A circle graph with more than 4 nodes" should "have maximal clique of size 3" in {
    for (i <- 5 to 10) {
      assert(Graph.maximalClique(circle(i)).size === 3)
      assert(Graph.bronKerbosch(circle(i)).size === 3)
    }
  }

  "A circle graph with 4 nodes" should "have maximal clique of size 4" in {
    assert(Graph.maximalClique(circle(4)).size === 4)
    assert(Graph.bronKerbosch(circle(4)).size === 4)
  }

  "A tree graph" should "have maximal clique of size 2" in {
    assert(Graph.maximalClique(tree).size === 2)
    assert(Graph.bronKerbosch(tree).size === 2)
  }

  "A maximalClique algorithm" should " more or less return within given timeout" in {
    val start: FiniteDuration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
    // it looks like time needed to build a tree becomes significant in case of larger graphs
    Graph.maximalClique(completeGraph(1000))(2 seconds)
    val end: FiniteDuration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
    assert(end - start < Duration(10L, TimeUnit.SECONDS))
  }
}
