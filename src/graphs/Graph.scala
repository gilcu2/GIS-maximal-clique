package graphs

import scala.concurrent.duration.Duration
import org.jgrapht.{UndirectedGraph => JGraphTUndirectedGraph}
import org.jgrapht.graph.{DefaultEdge => JGraphTDefaultEdge}
import org.jgrapht.graph.{SimpleGraph => JGraphTSimpleGraph}
import java.util.Collection
import scala.collection.JavaConversions._
import scala.util.Random
import scala.collection.immutable.IndexedSeq
import rx.lang.scala.Observable
import rx.lang.scala.subscriptions.Subscription
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.Some
import extensions.BronKerboschCliqueFinderExtended

/**
 * Generic Graph type where node can have any type V and edge have any type E
 *
 * This is functional data structure. Each modification operation yields new graph.
 *
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

/**
 * Concrete type V, here it is simple wrapper around Int
 * @param i node number (as defined by DIMACS)
 */
case class Node(i: Int)

/**
 * Concrete type E, here it is unweighted, undirected edge
 * @param v1 first node
 * @param v2 second node
 */
case class Edge(v1: Node, v2: Node)

/**
 * UndirectedGraph definition, concrete implementation of [[graphs.Graph]] where [[graphs.Graph.E]] is [[graphs.Edge]] and
 * [[graphs.Graph.V]] is [[graphs.Node]]
 *
 * UndirectedGraph is defined by two sets. Set of nodes and set of edges.
 * UndirectedGraph builds lazily adjacency lists (using map data structure for fast access given some node).
 * This means that adjacency list is only built the first time it is required so that upon construction of bigger and bigger
 * graphs has no overhead in repeated calculations. There is only single calculation on done graph.
 *
 * @param nodes set of nodes in the graph
 * @param edges set of nodes in the graph
 */
class UndirectedGraph(nodes: Set[Node], edges: Set[Edge]) extends Graph {

  type E = Edge
  type V = Node

  type Algorithm = (UndirectedGraph) => Set[Node]

  private def createAdjacencyMap(): Map[V, Set[V]] = {
    val emptyMapOfNodesToAdjacentNodes: Map[V, Set[V]] = (nodes :\ Map[V, Set[V]]())((node, map) => {
      map updated(node, Set[V]())
    })
    (edges :\ emptyMapOfNodesToAdjacentNodes)((edge, map) => {
      val map1 = map updated(edge.v1, map.getOrElse(edge.v1, Set[V]()) + edge.v2)
      map1 updated(edge.v2, map1.getOrElse(edge.v2, Set[V]()) + edge.v1)
    })
  }

  private lazy val adjacencyLists = createAdjacencyMap()

  /** Returns set of nodes adjacent to node v */
  def adj(v: V): Set[V] = adjacencyLists.getOrElse(v, Set[V]())

  /** Returns set of edges in this graph */
  def E: Set[E] = edges

  /** Returns set of nodes in this graph */
  def V: Set[V] = nodes

  /** Returns new graph with added Edge e */
  def addEdge(e: E): UndirectedGraph = new UndirectedGraph(nodes, edges + e)

  /** Returns new graph with added Node node */
  def addNode(node: V): UndirectedGraph = new UndirectedGraph(nodes + node, edges)

  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[UndirectedGraph] && obj.asInstanceOf[UndirectedGraph].V == this.V &&
    obj.asInstanceOf[UndirectedGraph].E == this.E

  /** Returns graph in format required by JGraphT library */
  def toJGraphT: JGraphTUndirectedGraph[Node, JGraphTDefaultEdge] = {
    var g: JGraphTUndirectedGraph[Node, JGraphTDefaultEdge] =
      new JGraphTSimpleGraph[Node, JGraphTDefaultEdge](classOf[JGraphTDefaultEdge])
    for (v <- nodes) g.addVertex(v)
    for (e <- edges) e match {
      case Edge(v1, v2) => g.addEdge(v1, v2)
      case _ =>
    }

    g
  }

  override def toString: String = "Nodes" + this.nodes + " Edges" + this.edges
}

/**
 * Result holder for each clique
 * @param nodes nodes forming clique that was found
 * @param elapsedTime Time elapsed in milliseconds
 * @param memoryInKb Memory used in kB
 */
case class CliqueFound(nodes: Set[Node], elapsedTime: Long, memoryInKb: Long)

object Graph {

  /** Implicit conversion from Int to Node */
  implicit def intToNode(i: Int) = Node(i)

  /** Implicit conversion from Tuple of ints to Edge */
  implicit def tupleToEdge(t: (Int, Int)) = Edge(t._1, t._2)

  /** Returns new empty undirected graph */
  def undirected(): UndirectedGraph = new UndirectedGraph(Set(), Set())

  /** Returns undirected graph with nodes and edges defined by given edges  */
  def undirected(edges: Set[Edge]): UndirectedGraph = {
    val nodes: Set[Node] = edges.flatMap(e => Set(e.v1, e.v2))
    new UndirectedGraph(nodes, edges)
  }

  /** Implicit no-op progress function */
  implicit val f: (Set[Node]) => Unit = (s) => ()

  /**
   * Runs Bron-Kerbosch algorithm on ready to go graph (graph in JGraphT format)
   *
   * @param g graph in JGrapT format
   * @param f progress function
   * @return set of nodes consisting of maximal clique
   */
  def bronKerboschWithReadyGraph(g: JGraphTUndirectedGraph[Node, JGraphTDefaultEdge])(implicit f: Set[Node] => Unit): Set[Node] = {
    val finder = new BronKerboschCliqueFinderExtended(g)
    val c: Collection[java.util.Set[graphs.Node]] =
      finder.getBiggestMaximalCliques(f)
    c.iterator().next().toSet
  }
  /** Runs Bron-Kerbosch algorithm on UndirectedGraph
    *
    * Does conversion and runs algorithm
    * */
  def bronKerbosch(g: UndirectedGraph)(implicit f: Set[Node] => Unit): Set[Node] = {
    val finder = new BronKerboschCliqueFinderExtended(g.toJGraphT)
    val c: Collection[java.util.Set[graphs.Node]] =
      finder.getBiggestMaximalCliques(f)
    c.iterator().next().toSet
  }

  /**
   * Runs algorithm on given graph and returns [[rx.lang.scala.Observable]] of results.
   *
   * * This method returns immediately. All the work is done in the background threads. Each next-best result is returned.
   * Refer to documentation on Observables for more context what it is.
   *
   * Use it like this:
   * {{{
   *   val observable: Observable[CliqueFound] = Graph.findBiggestClique(g, false, timeout)
      observable.subscribe(clique => {
          // code to execute on next clique found
          resultPrinter(clique)
      })
   * }}}
   *
   * @param g graph to run algorithm on
   * @param bronKerboschAlgorithm if true then Bron-Kerbosch algorithm is used, BasicMC otherwise
   * @param timeout timeout for algorithm. After that there are no more results and Observable is completed
   * @return
   */
  def findBiggestClique(g: UndirectedGraph, bronKerboschAlgorithm: Boolean, timeout: Duration): Observable[CliqueFound] = {
    Observable( observer => {

      var startTime = System.currentTimeMillis()
      var startMemory = Runtime.getRuntime().freeMemory
      val timer = () => System.currentTimeMillis() - startTime
      val memory = () => {
        val endMemory = Runtime.getRuntime().freeMemory
        (startMemory - endMemory) / 1024
      }

      val progress: (Set[Node] => Unit) = nodes => {
        observer.onNext(CliqueFound(nodes, elapsedTime = timer(), memoryInKb = memory()))
      }

      def initMetrics() = {
        delay(timeout).onSuccess { case _ => observer.onCompleted() }
        startMemory = Runtime.getRuntime().freeMemory
        startTime = System.currentTimeMillis()
      }

      val max: Future[Set[Node]] = if (bronKerboschAlgorithm) Future {
        val gg = g.toJGraphT
        initMetrics()
        bronKerboschWithReadyGraph(gg)(progress)
      } else Future {
        initMetrics()
        maximalClique(g)(progress)
      }

      max.onSuccess {
        case nodes => {
          progress(nodes)
          observer.onCompleted()
        }
      }

      Subscription {
        // Do nothing upon
      }
    })
  }

  /**
   * BasicMC algorithm
   * @param g graph to perform algorithm on
   * @param f function to which progress should be reported
   * @return set of nodes consisting of maximal clique
   */
  def maximalClique(g: UndirectedGraph)(implicit f: Set[Node] => Unit): Set[Node] = {
    type V = Node
    var Q = Set[V]()
    var Qmax = Set[V]()

    def expand(r: Set[V]): Set[V] = {
      var R = r
      while (R.nonEmpty) {
        val p = R.head
        if (Q.size + R.size > Qmax.size) {
          Q = Q + p
          val Rp = R intersect g.adj(p)
          if (Rp.nonEmpty) expand(Rp)
          else if (Q.size > Qmax.size) {
            Qmax = Q
            f(Qmax)
          }
          Q = Q - p
        }
        else Qmax
        R = R - p
      }
      Qmax
    }

    expand(g.V)
  }

  /**
   * Returns random undirected graph with specified properties
   *
   * @param n number of nodes
   * @param p probability of the edge
   * @return connected UndirectedGraph with number of edges very close to expected value
   */
  def randomUndirectedGraph(n: Int, p: Double): UndirectedGraph = {
    def edgesInGraph(n: Int) = (n * (n - 1)) / 2
    def expectedQ(n: Int, p: Double) = p * edgesInGraph(n)
    def createEdge(v1: Int, v2: Int) = if(v1 < v2) Some(Edge(v1, v2)) else if(v2 < v1) Some(Edge(v2, v1)) else None
    val t = (2 to n).:\((List(Node(1)), Set[Edge]()))((i, t) => (Node(i) :: t._1, {
      createEdge(t._1(Random.nextInt(t._1.size)).i, i).map(t._2 +).getOrElse(t._2)
    }))
    val nodes = t._1
    val connected: Set[Edge] = t._2
    val seq: IndexedSeq[Edge] = for {
      i <- 1 to n
      j <- 2 to n
      e <- createEdge(i, j)
    } yield e

    val edgesToAdd: IndexedSeq[Edge] = seq.filter(e => !(connected.contains(Edge(e.v1, e.v2)) || connected.contains(Edge(e.v2, e.v1)))).filter(_ => Random.nextDouble() <= p ).take((expectedQ(n, p) - connected.size).toInt)
    new UndirectedGraph(nodes.toSet, connected ++ edgesToAdd)
  }

  /** Private utility method which allows to define future that should be satisifed after some delay */
  private def delay(t: Duration): Future[Unit] = {
    val p = Promise[Unit]()
    val n = never[Unit]
    Future {
      try {
        Await.ready(n, t)
      }
      catch {
        case t: TimeoutException => p.success()
      }
    }

    p.future
  }

  /** Returns a future that is never completed.
    *
    *  This future may be useful when testing if timeout logic works correctly.
    */
  private def never[T]: Future[T] = {
    val p = Promise[T]()
    p.future
  }

}
