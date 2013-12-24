package app

import graphs._
import graphs.Graph._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import java.lang.Runtime

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

  def measureTimeAndMemory[R](codeBlock: () => R): (R, Long, Long) = {
    val start = System.currentTimeMillis()
    val startMemory = Runtime.getRuntime().freeMemory

    val result = codeBlock()

    val endMemory = Runtime.getRuntime().freeMemory
    val time = System.currentTimeMillis() - start
    val kb = (startMemory - endMemory)/1024
    (result, time, kb)
  }

  def measureTimeAndMemoryComplexity() = {
    for (n <- 10 to 40) {
      val sampleSize = 4
      var durationSum: Long = 0
      var memorySum: Long = 0
      for (j <- 1 to sampleSize) {
        val g = Graph.randomUndirectedGraph(n, 0.8)
        val (result, duration, memory) = measureTimeAndMemory( () => Graph.bronKerbosch(g))
        durationSum += duration
        memorySum += memory
      }

      val avgDuration: Double = durationSum/sampleSize
      val avgMemory: Double = memorySum/sampleSize

      println(s"${n} ${avgDuration} ${avgMemory}")
    }
  }


  override def main(args: Array[String]): Unit = {
    println("Begin reading stdin")
    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val dimacsGraph: DimacsGraph = readDimacsFormat(lines)
    println("Dimacs graph has been read. Proceeding to build graph")
    val g: UndirectedGraph = Graph.undirected(dimacsGraph.edges)
    println("Graph has been built. Triggering building of adjacency list")
    g.adj(Node(dimacsGraph.nodes)) // Trigger building of adjacency list
    println("Adjacency list is done. Begin maximal clique algorithm")
    val start: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
    val clique: Set[Node] = Graph.maximalClique(g)
    val time: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS) - start
    println(s"graph w(g) TIME\n${dimacsGraph.name} ${clique.size} ${time.toSeconds}")
  }
}
