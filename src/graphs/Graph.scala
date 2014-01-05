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
    (edges :\ emptyMapOfNodesToAdjacentNodes)((edge, map) => {
      val map1 = map updated(edge.v1, map.getOrElse(edge.v1, Set[V]()) + edge.v2)
      map1 updated(edge.v2, map1.getOrElse(edge.v2, Set[V]()) + edge.v1)
    })
  }

  private lazy val adjacencyLists = createAdjacencyMap()

  def adj(v: V): Set[V] = adjacencyLists.getOrElse(v, Set[V]())

  def E: Set[E] = edges

  def V: Set[V] = nodes

  def addEdge(e: E): UndirectedGraph = new UndirectedGraph(nodes, edges + e)

  def addNode(node: V): UndirectedGraph = new UndirectedGraph(nodes + node, edges)

  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[UndirectedGraph] && obj.asInstanceOf[UndirectedGraph].V == this.V &&
    obj.asInstanceOf[UndirectedGraph].E == this.E

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

case class CliqueFound(size: Long, elapsedTime: Long, memoryInKb: Long)

object Graph {

  implicit def intToNode(i: Int) = Node(i)

  implicit def tupleToEdge(t: (Int, Int)) = Edge(t._1, t._2)

  def undirected(): UndirectedGraph = new UndirectedGraph(Set(), Set())

  def undirected(edges: Set[Edge]): UndirectedGraph = {
    val nodes: Set[Node] = edges.flatMap(e => Set(e.v1, e.v2))
    new UndirectedGraph(nodes, edges)
  }

  implicit val f: (Set[Node]) => Unit = (s) => ()

  def bronKerbosch(g: UndirectedGraph)(implicit f: Set[Node] => Unit): Set[Node] = {
    val finder = new BronKerboschCliqueFinderExtended(g.toJGraphT)
    val c: Collection[java.util.Set[graphs.Node]] =
      finder.getBiggestMaximalCliques(f)
    c.iterator().next().toSet
  }


  def findBiggestClique(g: UndirectedGraph, bronKerboschAlgorithm: Boolean, timeout: Duration): Observable[CliqueFound] = {
    Observable( observer => {

      delay(timeout).onSuccess { case _ => observer.onCompleted() }
      def getTimer = {
        val start = System.currentTimeMillis()
        () => System.currentTimeMillis() - start
      }
      def getMemory = {
        val startMemory = Runtime.getRuntime().freeMemory
        () => {
          val endMemory = Runtime.getRuntime().freeMemory
          (startMemory - endMemory) / 1024
        }
      }
      val timer = getTimer
      val memory = getMemory

      val progress: (Set[Node] => Unit) = nodes => {
        observer.onNext(CliqueFound(nodes.size, elapsedTime = timer(), memoryInKb = memory()))
      }

      val max: Future[Set[Node]] = if (bronKerboschAlgorithm) Future {
        bronKerbosch(g)(progress)
      } else Future {
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
   * BasicMC
   * @param g - undirected graph
   * @return best clique so far found within given timeout
   */
  def maximalClique(g: UndirectedGraph)(implicit f: Set[Node] => Unit): Set[Node] = {
//    val start: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)

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
//            println("Best found so far: w(g)=" + Qmax.size + ". Time elapsed " + (Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS) - start).toSeconds + "s")
            f(Qmax)
          }
          Q = Q - p
        }
        else Qmax
        R = R - p

//        if (Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS) - start > timeout) {
//          return Qmax
//        }
      }
      Qmax
    }

    expand(g.V)
  }

  def randomUndirectedGraph(n: Int, p: Double): UndirectedGraph = {
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

    val edgesToAdd: IndexedSeq[Edge] = seq.filter(_ => Random.nextDouble() <= p )
    new UndirectedGraph(nodes.toSet, connected ++ edgesToAdd)
  }

  def delay(t: Duration): Future[Unit] = {
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
  def never[T]: Future[T] = {
    val p = Promise[T]()
    p.future
  }

}
