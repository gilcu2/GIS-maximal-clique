/**
 * Provides classes which define what a Graph is, functions which allow to operate on graph. Core of the program is defined here.
 *
 * Most important function is [[graphs.Graph.findBiggestClique()]] which performs algorithm and reports on its results.
 * This function returns immediately. All the work in done in the background thread. Results are reported asynchronously.
 *
 * Can be used like this:
 *
 * {{{
 *   val observable: Observable[CliqueFound] = Graph.findBiggestClique(g, false, timeout)
      observable.subscribe(clique => {
          // code to execute on next clique found
          resultPrinter(clique)
      })
 * }}}
 *
 * Refer to documentation on [[rx.lang.scala.Observable]] for more information on how to deal with results.
 *
 *
 */
package object graphs {

}
