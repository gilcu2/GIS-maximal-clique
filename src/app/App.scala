package app

import graphs._
import graphs.Graph._
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import java.lang.Runtime
import scala.util.Try
import rx.lang.scala.Observable

/** Main entry point to the application. Reads program arguments and acts accordingly.
  *
  *
  * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
  * @since 12/23/13
  */
object App extends scala.App {

  type Algorithm = (UndirectedGraph) => Set[Node]


  /**
    * DIMACS graph with optional metadata for ease of construction [[graphs.UndirectedGraph]]
    *
    * @param name name of the dimacs graph
    * @param nodes number of nodes in graph
    * @param totalEdges number of edges in graph
    * @param edges definitions of edges in the graph
   */
  case class DimacsGraph(name: String = "", nodes: Int = -1, totalEdges: Int = -1, edges: Set[Edge] = Set()) {
    /** Returns true if graph has been successfully read */
    def isDefined = edges.nonEmpty
  }

  /**
   * Returns [[app.App.DimacsGraph]] read from some stream.
   *
   * To read from stdin use it like so:
   * {{{
   *  val lines: Iterator[String] = scala.io.Source.stdin.getLines()
     val dimacsGraph: DimacsGraph = Try(readDimacsFormat(lines)).getOrElse(DimacsGraph())
   * }}}
   *
   * Wrapping it with [[scala.util.Try]] allows for safe construction
   *
   * @param lines input graph
   * @return DimacsGraph
   */
  def readDimacsFormat(lines: Iterator[String]): DimacsGraph = {
    var dimacsGraph = DimacsGraph()
    val EdgeLine = """e (\d*) (\d*)""".r
    val FileName = """c (FILE|File):?\s*(\S*)\s*""".r
    val NodesAndEdges = """p col (\d*) (\d*)""".r
    for (line <- lines) {
      line match {
        case EdgeLine(from, to) => {
          dimacsGraph = dimacsGraph.copy(edges = dimacsGraph.edges + Edge(Node(from.toInt), Node(to.toInt)))
        }
        case FileName(_, name) => dimacsGraph = dimacsGraph.copy(name = name)
        case NodesAndEdges(nodes, edges) => dimacsGraph = dimacsGraph.copy(nodes = nodes.toInt, totalEdges = edges.toInt)
        case _ =>
      }
    }
    dimacsGraph
  }

  /**
   * Returns time in milliseconds and memory in kb required to execute passed in codeBlock
   * @param codeBlock code block to execute
   * @tparam R any result
   * @return triple, result of the code block along with time and memory
   */
  def measureTimeAndMemory[R](codeBlock: () => R): (R, Long, Long) = {
    val start = System.currentTimeMillis()
    val startMemory = Runtime.getRuntime().freeMemory

    val result = codeBlock()

    val endMemory = Runtime.getRuntime().freeMemory
    val time = System.currentTimeMillis() - start
    val kb = (startMemory - endMemory) / 1024
    (result, time, kb)
  }

  /** BenchmarkResult is simple wrapper for benchmark results.
   *
   * @param n number of nodes
   * @param avgDuration average duration of algorithm in milliseconds
   * @param avgMemory average memory taken by algorithm in kb
   */
  case class BenchmarkResult(n: Int, avgDuration: Double, avgMemory: Double) {
    override def toString = {
      val s1 = n.toString
      val s2 = avgDuration.toString
      val s3 = avgMemory.toString
      List(s1, s2, s3).mkString(",")
    }
  }

  /**
   * Performs benchmark and returns its results as list. Benchmark is performed on random undirected graph with given parameters.
   *
   * Benchmarking starts from graphs with 10 nodes up to given maximal number of nodes specified in appOptions.
   * Number of nodes and probability of the edge parameters are used to construct random undirected graph.
   *
   * For each constructed graph algorithm is run 4 times (sampleSize) and results of benchmark are aggregated and averaged.
   *
   *
   * @param maxNodes maximal number of nodes. Benchmark will be run on graphs starting at 10 nodes up to maxNodes
   * @param probabilityOfEdge probability of the edge
   * @param applyAlgorithm function which applies algorithm to given graph
   * @return list of [[app.App.BenchmarkResult]]
   */
  def measureTimeAndMemoryComplexity(maxNodes: Int,
                                     probabilityOfEdge: Double,
                                     applyAlgorithm: Algorithm) = {
    var results = scala.collection.mutable.MutableList[BenchmarkResult]()
    for (n <- 10 to maxNodes) {
      val sampleSize = 4
      var durationSum: Long = 0
      var memorySum: Long = 0
      for (j <- 1 to sampleSize) {
        val g = Graph.randomUndirectedGraph(n, probabilityOfEdge)
        val (result, duration, memory) = measureTimeAndMemory(() => applyAlgorithm(g))
        durationSum += duration
        memorySum += memory
      }

      val avgDuration: Double = durationSum / sampleSize
      val avgMemory: Double = memorySum / sampleSize

      results += BenchmarkResult(n, avgDuration, avgMemory)
    }
    results.toList
  }

  def measureAlgorithmPerformance(g: UndirectedGraph,
                                  algorithm: Algorithm) = {

    val (result, duration, memory) =
      measureTimeAndMemory(() => algorithm(g))
    BenchmarkResult(g.V.size, duration, memory)
  }

  def measureAverageAlgorithmPerformance(g: UndirectedGraph,
                                         algorithm: Algorithm,
                                         samples: Int) = {
    val results = (1 to samples).map(i =>
      measureAlgorithmPerformance(g, algorithm))
    val time = results.map(_.avgDuration)
    val memory = results.map(_.avgMemory)
    val avgTime = time.foldLeft(0.0)(_+_) / samples
    val avgMemory = memory.foldLeft(0.0)(_+_) / samples
    BenchmarkResult(g.V.size, avgTime, avgMemory)
  }

  def benchmark(maxNodes: Int,
                probabilityOfEdge: Double): List[String]  = {
    val bronKerbosch = g => Graph.bronKerbosch(g)
    val basicMc = g => Graph.maximalClique(g)
    (10 to maxNodes).map( (n) => {
      val g = Graph.randomUndirectedGraph(n, probabilityOfEdge)
      List("Bron-Kerbosch," + measureAverageAlgorithmPerformance(g, bronKerbosch, 4).toString,
      "BasicMC," + measureAverageAlgorithmPerformance(g, basicMc, 4))
    }).flatten.toList
  }
  /**
   * Program usage instructions, linux like style.
   */
  val usage =
    """ Usage:
      | [-j] [-max seconds] [-v] [-csv] [-benchmark maxNodes probabilityOfEdge] [-progress]
      |
      |
      |
      | -j Use Bron-Kerbosch algorithm. Uses BasicMC by default
      |
      | -max seconds - run algorithm for maximum number of seconds. Best result so far will be returned upon timeout
      |
      | -v verbose mode
      |
      | -csv Output results in csv like format. Result are in format: GraphName, Algorithm, w(g), TIME, memory
      |
      | -benchmark maxNodes probabilityOfEdge Perform benchmark of algorithm using randomly generated graphs with specified parameters:
      | max nodes and probability of the edge. Benchmark is another mode to run program. You can't benchmark some dimacs graph.
      | maxNodes should be greater than 10
      |
      | -progress output intermediate maximal cliques
      |
      | -sizeOnly do not display labels of nodes forming maximal clique
      |
      | Example
      | "-max 90 -progress -csv < data/graph" Runs BasicMC algorithm for maximum of 90 seconds. Displays next best results (progress) using csv format
      | "-max 30 -j < data/graph >> results" Runs Bron-Kerbosch algorithm for maximum of 30 seconds
      | "-benchmark 100 0.9" Does benchmark on random undirected graphs starting at 10 nodes up to 100 nodes with
      |  probability of edge 0.9 using BasicMC algorithm
      | "-benchmark 50 0.7 -j" Does benchmark on random undirected graphs starting at 10 nodes up to 50 nodes with
      |  probability of edge 0.7 using Bron-Kerbosch algorithm
    """.stripMargin

  /**
   * Application options are grouped into this single case class
   * @param bronKerbosch if true use Bron-Kerbosch algorithm. Default is false
   * @param timeout maximum number of seconds. Defaults to infinity
   * @param benchmark if true then run benchmark mode
   * @param benchmarkMaxNodes max nodes for benchmark mode
   * @param probabilityOfEdge probability of edge for benchmark mode
   * @param verbose if true then program will be verbose
   * @param outputInCSVFormat if true then results will be printed in csv like style: GraphName, Algorithm, w(g), TIME, memory
   * @param showProgress if true then intermediate results will be printed on the standard output
   */
  case class AppOptions(bronKerbosch: Boolean = false, timeout: Duration = Duration.Inf, benchmark: Boolean = false, benchmarkMaxNodes: Int = 40,
                        probabilityOfEdge : Double = 0.8, verbose: Boolean = false, outputInCSVFormat: Boolean = false,
                        showProgress: Boolean = false, sizeOnly: Boolean = false)

  /** Helper method to read program arguments and build instance of AppOptions */
  private def nextOption(appOptions: AppOptions, remainingArgs: List[String]): AppOptions = {
    remainingArgs match {
      case "-j" :: tail => nextOption(appOptions.copy(bronKerbosch = true), tail)
      case "-max" :: timeoutInSeconds :: tail => nextOption(appOptions.copy(timeout = Duration(timeoutInSeconds.toInt, TimeUnit.SECONDS)), tail)
      case "-v" :: tail => nextOption(appOptions.copy(verbose = true), tail)
      case "-csv" :: tail => nextOption(appOptions.copy(outputInCSVFormat = true), tail)
      case "-benchmark" :: maxN :: probabilityOfEdge :: tail => nextOption(appOptions.copy(benchmark = true, benchmarkMaxNodes = maxN.toInt,probabilityOfEdge =  probabilityOfEdge.toDouble), tail)
      case "-progress" :: tail => nextOption(appOptions.copy(showProgress = true), tail)
      case "-sizeOnly" :: tail => nextOption(appOptions.copy(sizeOnly = true), tail)
      case rest => appOptions
    }
  }

  private val appOptions: AppOptions = nextOption(AppOptions(), args.toList)


  if (appOptions.benchmark) {
    if(appOptions.benchmarkMaxNodes < 10) {
      println("ILLEGAL ARGUMENT VALUE -benchmark maxNodes. MaxNodes should be greater than 10")
      println()
      println(usage)
    }
    else {
      val printableResults = benchmark(appOptions.benchmarkMaxNodes,
                                       appOptions.probabilityOfEdge)
      println(printableResults.mkString("\n"))
    }
  }
  else if(System.in.available() > 0) {
    def getProgressPrinter(verbose: Boolean) = (s: String) => if (verbose) println(s)
    def getResultPrinter(csvOutput: Boolean, graphName: String, bronKerbosch: Boolean, sizeOnly: Boolean) =
      (cf: CliqueFound) => if(csvOutput) {
        val listEnd = if(sizeOnly) Nil else cf.nodes.map(_.i).mkString(" ") :: Nil
        val l = graphName :: (if(bronKerbosch) "Bron-Kerbosch" else "BasicMC") :: cf.nodes.size :: cf.elapsedTime ::
          cf.memoryInKb :: listEnd
        println(l.mkString(","))
      }
      else
      {
        println(s"${cf.nodes.size} ${cf.elapsedTime}")
        if(!sizeOnly) println(cf.nodes.map(_.i).mkString(" "))
      }

    val printer: (String) => Unit = getProgressPrinter(appOptions.verbose)



    printer("Begin reading stdin")

    val lines: Iterator[String] = scala.io.Source.stdin.getLines()
    val dimacsGraph: DimacsGraph = Try(readDimacsFormat(lines)).getOrElse(DimacsGraph())

    if (dimacsGraph.isDefined) {
      val resultPrinter = getResultPrinter(appOptions.outputInCSVFormat, dimacsGraph.name, appOptions.bronKerbosch, appOptions.sizeOnly)
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
        val maximal = list.headOption.getOrElse(CliqueFound(Set(),appOptions.timeout.toMillis,0))
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
