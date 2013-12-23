package app

import graphs._
import graphs.Graph._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
object App extends scala.App {

  case class DimacsGraph(name: String = "", nodes: Int = -1, totalEdges: Int = -1, edges: Set[Edge] = Set())

  def readDimacsFormat(lines: Iterator[String]): DimacsGraph = {
    var dimacsGraph = DimacsGraph()
    val EdgeLine = """e (\d*) (\d*)""".r
    val FileName = """c FILE:\s*(\S*)""".r
    val NodesAndEdges = """p col (\d*) (\d*)""".r
    for (line <- lines) {
      line match {
        case EdgeLine(from, to) => {
          dimacsGraph = dimacsGraph.copy(edges = dimacsGraph.edges + Edge(Node(from.toInt), Node(to.toInt)))
        }
        case FileName(name) => dimacsGraph = dimacsGraph.copy(name = name)
        case NodesAndEdges(nodes, edges) => dimacsGraph = dimacsGraph.copy(nodes = nodes.toInt, totalEdges = edges.toInt)
        case _ =>
      }
    }
    dimacsGraph
  }


  override def main(args: Array[String]): Unit = {
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val dimacsGraph: DimacsGraph = readDimacsFormat(lines)
    val g: UndirectedGraph = Graph.undirected(dimacsGraph.edges)
    g.adj(Node(dimacsGraph.nodes)) // Trigger building of adjacency list
    val start: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
    val clique: Set[Node] = Graph.maximalClique(g)
    val time: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS) - start

    println(s"graph w(g) TIME\n${dimacsGraph.name} ${clique.size} ${time.toSeconds}")
  }
}
