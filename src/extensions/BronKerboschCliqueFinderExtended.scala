package extensions


import org.jgrapht.{Graph => JGraphTGraph}
import java.util
import scala.collection.JavaConversions._
import scala.util.Try

/**
 * @author Marek Lewandowski <marek.m.lewandowski@gmail.com>
 * @since 1/5/14
 */
class BronKerboschCliqueFinderExtended[V, E](graph: JGraphTGraph[V, E]) extends ExtendableBronKerboschCliqueFinder[V, E](graph) {

  var progressFunc: Set[V] => Unit = (s) => ()

  protected override def nextCliqueFound(clique: util.Set[V]) {
    Try(progressFunc(clique.toSet)).toOption
  }

  def getBiggestMaximalCliques(progress: Set[V] => Unit): List[Set[V]] = {
    progressFunc = progress
    super.getBiggestMaximalCliques.map(_.toSet).toList
  }
}
