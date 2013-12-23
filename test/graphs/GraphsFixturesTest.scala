package graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.Matchers
import Graph._

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
class GraphsFixturesTest extends FlatSpec with Matchers with GraphsFixtures {


  "A cycle 3 fixture" should "produce cycle graph" in {
    val c3: UndirectedGraph = cycle(3)

    val g2: UndirectedGraph = Graph.undirected().addNode(1).addNode(2).addNode(3).addEdge((1, 2)).addEdge((2, 3)).addEdge((1, 3))

    assert(c3 == g2)
  }

  "A cycle 4 fixture" should "produce cycle graph" in {
    val c4: UndirectedGraph = cycle(4)

    val g2: UndirectedGraph = Graph.undirected().addNode(1).addNode(2).addNode(3).addNode(4).addEdge((1, 2)).addEdge((2, 3)).addEdge((3, 4)).addEdge((1, 4))

    assert(c4 == g2)
  }
}
