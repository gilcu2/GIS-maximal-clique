package app

import graphs._
import graphs.Graph._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import java.lang.Runtime
import scala.util.Try
import rx.lang.scala.Observable

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
      | [-j] [-max seconds] [-v] [-csv] [-benchmark] [-progress]
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
      | -progress output intermediate maximal cliques
      |
      |
      | Example
      | "-max 30 -j < data/graph >> results" Runs Bron-Kerbosch algorithm for maximum of 30 seconds
    """.stripMargin

  case class AppOptions(bronKerbosch: Boolean = false, timeout: Duration = Duration.Inf, benchmark: Boolean = false,
                        verbose: Boolean = false, outputInCSVFormat: Boolean = false, showProgress: Boolean = false)

  def nextOption(appOptions: AppOptions, remainingArgs: List[String]): AppOptions = {
    remainingArgs match {
      case "-j" :: tail => nextOption(appOptions.copy(bronKerbosch = true), tail)
      case "-max" :: timeoutInSeconds :: tail => nextOption(appOptions.copy(timeout = Duration(timeoutInSeconds.toInt, TimeUnit.SECONDS)), tail)
      case "-v" :: tail => nextOption(appOptions.copy(verbose = true), tail)
      case "-csv" :: tail => nextOption(appOptions.copy(outputInCSVFormat = true), tail)
      case "-benchmark" :: tail => nextOption(appOptions.copy(benchmark = true), tail)
      case "-progress" :: tail => nextOption(appOptions.copy(showProgress = true), tail)
      case rest => appOptions
    }
  }

  private val appOptions: AppOptions = nextOption(AppOptions(), args.toList)


  if (appOptions.benchmark) {
    val algorithm: (UndirectedGraph) => Set[Node] = if(appOptions.bronKerbosch) g => Graph.bronKerbosch(g) else g => Graph.maximalClique(g)
    measureTimeAndMemoryComplexity(algorithm)
  }
  else if(System.in.available() > 0) {
    def getProgressPrinter(verbose: Boolean) = (s: String) => if (verbose) println(s)
    def getResultPrinter(csvOutput: Boolean, graphName: String, bronKerbosch: Boolean) =
      (cf: CliqueFound) => if(csvOutput) {
        val l = graphName :: (if(bronKerbosch) "Bron-Kerbosch" else "BasicMQ") :: cf.size :: cf.elapsedTime :: cf.memoryInKb :: Nil
        println(l.mkString(","))
      }
      else
        println(s"${cf.size} ${cf.elapsedTime}")

    val printer: (String) => Unit = getProgressPrinter(appOptions.verbose)



    printer("Begin reading stdin")

    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val dimacsGraph: DimacsGraph = Try(readDimacsFormat(lines)).getOrElse(DimacsGraph())

    if (dimacsGraph.isDefined) {
      val resultPrinter = getResultPrinter(appOptions.outputInCSVFormat, dimacsGraph.name, appOptions.bronKerbosch)
      printer("Dimacs graph has been read. Proceeding to build graph")
      val g: UndirectedGraph = Graph.undirected(dimacsGraph.edges)
      printer("Graph has been built. Triggering building of adjacency list")
      g.adj(Node(dimacsGraph.nodes)) // Trigger building of adjacency list
      printer("Adjacency list is done. Begin maximal clique algorithm")

      val observable: Observable[CliqueFound] = Graph.findBiggestClique(g, appOptions.bronKerbosch, appOptions.timeout)
      if(appOptions.showProgress)
        observable.subscribe(c => {
          resultPrinter(c)
        })
      val list: List[CliqueFound] = observable.toBlockingObservable.toList
      if(!appOptions.showProgress) {
        val maximal = list.headOption.getOrElse(CliqueFound(1,appOptions.timeout.toMillis,0))
        resultPrinter(maximal)
      }
    }
    else {
      println("ILLEGAL FORMAT! Expected graph in dimacs format")
      println()
      println(usage)
    }
  }
  else {
    println("NO INPUT FOUND! Expected graph in dimacs format on standard input")
    println()
    println(usage)
  }


}
