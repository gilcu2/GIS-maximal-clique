package app

import graphs._
import graphs.Graph._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import java.lang.Runtime
import scala.util.Try

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 12/23/13
 */
object App extends scala.App {

  case class DimacsGraph(name: String = "", nodes: Int = -1, totalEdges: Int = -1, edges: Set[Edge] = Set()) {
    def isDefined = edges.nonEmpty
  }

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
    val kb = (startMemory - endMemory) / 1024
    (result, time, kb)
  }

  def measureTimeAndMemoryComplexity(applyAlgorithm: (UndirectedGraph) => Set[Node]) = {
    for (n <- 10 to 40) {
      val sampleSize = 4
      var durationSum: Long = 0
      var memorySum: Long = 0
      for (j <- 1 to sampleSize) {
        val g = Graph.randomUndirectedGraph(n, 0.8)
        val (result, duration, memory) = measureTimeAndMemory(() => applyAlgorithm(g))
        durationSum += duration
        memorySum += memory
      }

      val avgDuration: Double = durationSum / sampleSize
      val avgMemory: Double = memorySum / sampleSize

      println(s"${n} ${avgDuration} ${avgMemory}")
    }
  }

  def measureTimeAndMemoryComplexityOfBronKerbosch() = {
    measureTimeAndMemoryComplexity(g => Graph.bronKerbosch(g))
  }

  val usage =
    """ Usage:
      | [-j] [-max seconds] [-v] [-csv] [-benchmark]
      |
      |
      |
      | -j Use Bron-Kerbosch algorithm. Uses BasicMQ by default
      |
      | -max seconds - run algorithm for maximum number of seconds. Best result so far will be returned upon timeout
      |
      | -v verbose mode
      |
      | -csv Output results in csv like format
      |
      | -benchmark Perform benchmark of algorithms using randomly generated graphs
      |
      |
      | Example
      | "-max 30 -j << data/graph >> results" Runs Bron-Kerbosch algorithm for maximum of 30 seconds
    """.stripMargin

  case class AppOptions(bronKerbosch: Boolean = false, timeout: Duration = Duration.Inf, benchmark: Boolean = false,
                        verbose: Boolean = false, outputInCSVFormat: Boolean = false)

  def nextOption(appOptions: AppOptions, remainingArgs: List[String]): AppOptions = {
    remainingArgs match {
      case "-j" :: tail => nextOption(appOptions.copy(bronKerbosch = true), tail)
      case "-max" :: timeoutInSeconds :: tail => nextOption(appOptions.copy(timeout = Duration(timeoutInSeconds.toInt, TimeUnit.SECONDS)), tail)
      case "-v" :: tail => nextOption(appOptions.copy(verbose = true), tail)
      case "-csv" :: tail => nextOption(appOptions.copy(outputInCSVFormat = true), tail)
      case "-benchmark" :: tail => nextOption(appOptions.copy(benchmark = true), tail)
      case rest => appOptions
    }
  }

  def getProgressPrinter(verbose: Boolean) = (s: String) => if (verbose) println(s)

  private val appOptions: AppOptions = nextOption(AppOptions(), args.toList)
  private val printer: (String) => Unit = getProgressPrinter(appOptions.verbose)

  if (appOptions.benchmark) {
    // TODO
  }
  else {
    printer("Begin reading stdin")

    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val dimacsGraph: DimacsGraph = Try(readDimacsFormat(lines)).getOrElse(DimacsGraph())

    if (dimacsGraph.isDefined) {
      printer("Dimacs graph has been read. Proceeding to build graph")
      val g: UndirectedGraph = Graph.undirected(dimacsGraph.edges)
      printer("Graph has been built. Triggering building of adjacency list")
      g.adj(Node(dimacsGraph.nodes)) // Trigger building of adjacency list
      printer("Adjacency list is done. Begin maximal clique algorithm")
      val start: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)

      val (clique, time, memory) =
        if (appOptions.bronKerbosch)
          measureTimeAndMemory(() => Graph.bronKerbosch(g))
        else
          measureTimeAndMemory(() => Graph.maximalClique(g))

      val elapsedTime: Duration = Duration(System.currentTimeMillis(), TimeUnit.MILLISECONDS) - start
      printer(s"graph w(g) TIME\n${dimacsGraph.name} ${clique.size} ${elapsedTime.toSeconds}")
    }
    else {
      println("MISSING INPUT. Expected graph in dimacs format")
      println()
      println(usage)
    }

  }


}
