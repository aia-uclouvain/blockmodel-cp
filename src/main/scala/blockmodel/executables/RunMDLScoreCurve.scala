package blockmodel.executables

import blockmodel.{Blockmodel, BlockmodelCPModel}
import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.{Digraph, FileReporter}
import blockmodel.utils.Matrix._
import oscar.cp._

import math.log

object RunMDLScoreCurve extends App {
  val g = Digraph.fromTSV(args(0))
  val n = g.n
  val maxK = args(1).toInt

  val res = new StringBuilder()
  res append "k\tmdl\tmodel\terror\tcost\n"

  for (k <- 1 to maxK) {
    object model extends BlockmodelCPModel(g, k) {
      var bestScore = totalCost.getMax
      var bestBM: Blockmodel = null
      val decisionVars: Array[CPIntVar] = C ++ M.cells
      minimize(totalCost)
      search {
        PermutationBreakingBranching(C, blockmodelConstraint.biggestCostDelta, identity) ++ binaryMaxWeightedDegree(M.flatten.asInstanceOf[Array[CPIntVar]])
      }
      onSolution {
        val solC: Array[Int] = C.map(_.value)
        val solM: Array[Array[Boolean]] = M.mapCells(_.value == 1)
        val blockmodel = new Blockmodel(solC, solM)
        bestBM = blockmodel
        bestScore = totalCost.value
      }
      solver.silent = true
      val stats = start()
    }
    println(s"k = $k")
    println(model.stats)
    val mdl_model = log(n)/log(2) + n * log(k)/log(2) + k*k + log(n*n)/log(2)
    val mdl_error = logC(n*n, model.bestScore)/log(2)
    val mdl = mdl_model + mdl_error
    println(mdl)
    res append s"$k\t$mdl\t$mdl_model\t$mdl_error\t${model.bestScore}\n"
  }

  new FileReporter(res.result(), args(2))

  def logC(n: Int, k: Int): Double = {
    if (k < 0 || k > n)   throw new IllegalArgumentException()
    if (k == 0 || k == n) return 0
    val kmin = math.min(k, n - k)
    var c = 0.0
    for (i <- 0 until kmin) {
      c = c + log(n - i) - log(i + 1)
    }
    c
  }
}
