package blockmodel.executables

import blockmodel.{Blockmodel, BlockmodelCPModel}
import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.{Digraph, FileReporter}
import blockmodel.utils.Matrix._
import oscar.algo.search.DefaultDFSearchListener
import oscar.cp._

import math.log

object RunMDLScoreCurve extends App {
  val g = Digraph.fromTSV(args(0))
  val n = g.n
  val maxK = args(1).toInt


  new FileReporter("k\tmdl\tmodel\terror\tcost", args(2), true)

  var lastBest = 4900
  for (k <- 16 to maxK) {
    object model extends BlockmodelCPModel(g, k) {
      var bestScore = totalCost.getMax
      var bestBM: Blockmodel = null
      val decisionVars: Array[CPIntVar] = C ++ M.cells
      add(totalCost < lastBest)
      minimize(totalCost)
      search {
        PermutationBreakingBranching(C, blockmodelConstraint.biggestCostDelta, (_,_)=>0) ++ binaryMaxWeightedDegree(M.flatten.asInstanceOf[Array[CPIntVar]])
        //binaryMaxWeightedDegree(C) ++ binaryMaxWeightedDegree(M.flatten.asInstanceOf[Array[CPIntVar]])
      }
      onSolution {
        println("found sol")
        val blockmodel = getBlockmodel()
        bestBM = blockmodel
        bestScore = totalCost.value
      }
      val stats = start()
    }
    println(s"k = $k")
    println(model.stats)
    if (model.stats.nSols > 0) lastBest = model.bestScore
    val mdl_model = log(n)/log(2) + n * log(k)/log(2) + k*k + log(n*n)/log(2)
    val mdl_error = logC(n*n, model.bestScore)/log(2)
    val mdl = mdl_model + mdl_error
    println(mdl)
    new FileReporter(s"$k\t$mdl\t$mdl_model\t$mdl_error\t${model.bestScore}", args(2), true)
  }

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
