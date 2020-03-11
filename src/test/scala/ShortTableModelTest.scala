import blockmodel.utils.Digraph
import blockmodel.utils.Matrix._
import blockmodel.{BaselineBlockmodelCPModel, Blockmodel, BlockmodelCPModel, ShortTableModel}
import org.scalatest._
import oscar.algo.search.SearchStatistics
import oscar.cp._

import scala.collection.mutable

class ShortTableModelTest extends FunSuite {

  val rng = new scala.util.Random(0)

  class BaselineModel(g: Digraph, k: Int, maxCost: Int) extends BaselineBlockmodelCPModel(g, k) {
    add(totalCost <= maxCost)
    for (i <- 0 until k) {
      add(C(i) <= i)
    }

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
      binaryMaxWeightedDegree(decisionVars)
    }
    val stats: SearchStatistics = start()
  }

  class OurModel(g: Digraph, k: Int, maxCost: Int) extends ShortTableModel(g, k) {
    add(totalCost <= maxCost)
    for (i <- 0 until k) {
      add(C(i) <= i)
    }
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
      binaryMaxWeightedDegree(decisionVars)
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