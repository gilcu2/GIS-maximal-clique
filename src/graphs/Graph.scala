package graphs

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
trait Graph {
  type V
  type E

  def V: Set[V]

  def E: Set[E]

  def adj(v: V): Set[V]

  def addEdge(e: E): Graph

  def addNode(v: V): Graph

}

case class Node(i: Int)
case class Edge(v1: Node, v2: Node)

class UndirectedGraph(nodes: Set[Node], edges: Set[Edge]) extends Graph {

  type E = Edge
  type V = Node

  private def createAdjacencyMap(): Map[V, Set[V]] = {
    val emptyMapOfNodesToAdjacentNodes: Map[V, Set[V]] = (nodes :\ Map[V, Set[V]]())((node, map) => {
      map updated(node, Set[V]())
    })
    (edges :\ emptyMapOfNodesToAdjacentNodes)( (edge, map) => {
      val map1 = map updated (edge.v1, map.getOrElse(edge.v1, Set[V]()) + edge.v2)
      map1 updated(edge.v2, map1.getOrElse(edge.v2, Set[V]()) + edge.v1 )
    } )
  }

  private lazy val adjacencyLists = createAdjacencyMap()

  def adj(v: V): Set[V] = adjacencyLists.getOrElse(v, Set[V]())

  def E: Set[E] = edges

  def V: Set[V] = nodes

  def addEdge(e: E): UndirectedGraph = new UndirectedGraph(nodes, edges + e)

  def addNode(node: V): UndirectedGraph = new UndirectedGraph(nodes + node, edges)
}

object Graph {

  def undirected(): UndirectedGraph = new UndirectedGraph(Set(), Set())

}
