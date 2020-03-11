import blockmodel.utils.Digraph
import blockmodel.{BaselineBlockmodelCPModel, Blockmodel, BlockmodelCPModel}
import org.scalatest._
import oscar.algo.search.SearchStatistics
import oscar.cp._
import blockmodel.utils.Matrix._

import scala.collection.mutable

class BlockmodelCostTest extends FunSuite {

  val rng = new scala.util.Random(0)

  class BaselineModel(g: Digraph, k: Int, maxCost: Int) extends BaselineBlockmodelCPModel(g, k) {
    add(totalCost <= maxCost)

    val solutionSet: mutable.Set[Blockmodel] = mutable.Set[Blockmodel]()
    val decisionVars: Array[CPIntVar] = C ++ M.cells
    onSolution {
      val solC: Array[Int] = C.map(_.value)
      val solM: Array[Array[Boolean]] = M.mapCells(_.value == 1)
      val blockmodel = new Blockmodel(solC, solM)
      assert(blockmodel.cost(g) == totalCost.value)
      solutionSet.add(blockmodel)
    }
    search {
      conflictOrderingSearch(decisionVars, minDom(decisionVars), minVal(decisionVars))
    }
    val stats: SearchStatistics = start()
  }

  class OurModel(g: Digraph, k: Int, maxCost: Int) extends BlockmodelCPModel(g, k) {
    add(totalCost <= maxCost)

    val decisionVars: Array[CPIntVar] = C ++ M.cells
    val solutionSet: mutable.Set[Blockmodel] = mutable.Set[Blockmodel]()
    onSolution {
      solver.update()
      val solC: Array[Int] = C.map(_.value)
      val solM: Array[Array[Boolean]] = M.mapCells(_.value == 1)
      val blockmodel = new Blockmodel(solC, solM)
      if (!totalCost.isBound) {
        println(s"the cost is not bound... it is $totalCost")
        println("individual costs:")
        println(cost.toStringMatrix)
        println(blockmodel)
        println(blockmodel.toStringGrouped(g))
        println(g.adjacencyMatrix.toStringMatrix)

      }
      if (blockmodel.cost(g) != totalCost.value) {
        println(cost.toStringMatrix)
        println(blockmodel)
        println(blockmodel.toStringGrouped(g))
        println(g.adjacencyMatrix.toStringMatrix)
      }
      assert(blockmodel.cost(g) == totalCost.value)
      solutionSet.add(blockmodel)
    }
    search {
      conflictOrderingSearch(decisionVars, identity, minVal(decisionVars))
    }
    val stats: SearchStatistics = start()
  }

  test("BlockmodelCost against the naive implementation") {

    val n = 10
    val k = 3
    for (i <- 0 until 10) {
      println(s"test $i")
      val g = Digraph.random(n, rng)

      println("our model:")
      val our = new OurModel(g, k, n * n / 4)
      println(our.stats)

      println("baseline:")
      val baseline = new BaselineModel(g, k, n * n / 4)
      println(baseline.stats)
      val trustedSolutions = baseline.solutionSet
      val otherSolutions = our.solutionSet

      val falseSolutions = otherSolutions &~ trustedSolutions
      val unfoundSolutions = trustedSolutions &~ otherSolutions

      assert(falseSolutions.isEmpty, s"there were ${falseSolutions.size} false solutions.")
      assert(unfoundSolutions.isEmpty)

      assert(trustedSolutions == otherSolutions)
    }
  }
}