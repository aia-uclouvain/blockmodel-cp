package blockmodel.executables

import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.Matrix._
import blockmodel.utils.{BinomaialCoefficient, Digraph, FileReporter}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import oscar.cp._

import scala.math.log

object RunMDLScoreCurve2 extends App {
  val g = Digraph.fromTSV(args(0))
  val n = g.n
  val maxK = args(1).toInt


  val trackedValues = Seq("mdl", "model", "error", "cost", "base-nnodes", "base-time", "better-nnodes", "better-time",
    "best-nnodes", "best-time")

  val res = new StringBuilder()
  res append ("k\t" + trackedValues.mkString("\t") + "\n")

  val logCTable = BinomaialCoefficient.logCTable(n*n)
  var lastBestScore = Integer.MAX_VALUE
  var lastBestMDL =   Double.PositiveInfinity

  for (k <- 1 to maxK) {
    class model extends BlockmodelCPModel(g, k) {
      var bestCost: Option[Int] = None
      var bestBM: Option[Blockmodel] = None
      val decisionVars: Array[CPIntVar] = C ++ M.cells
      minimize(totalCost)
      search {
        PermutationBreakingBranching(C, blockmodelConstraint.biggestCostDelta, (_,_)=>0) ++ binaryMaxWeightedDegree(M.flatten.asInstanceOf[Array[CPIntVar]])
      }
      onSolution {
        bestBM = Some(getBlockmodel())
        bestCost = Some(totalCost.value)
      }
      solver.silent = true
    }

    println(s"k = $k")

    val baseModel = new model()
    val baseStats = baseModel.solver.startSubjectTo() {}
    println(baseStats)

    val betrModel = new model()
    val betrStats = betrModel.solver.startSubjectTo() {
      println(s"ub = $lastBestScore")
      betrModel.solver.add(betrModel.totalCost < lastBestScore)
    }
    println(betrStats)

    val bestModel = new model()
    val mdlModel = log(n) / log(2) + n * log(k) / log(2) + k * k + log(n * n) / log(2)
    val bestStats = bestModel.solver.startSubjectTo() {
      val ub = BinomaialCoefficient.search(logCTable, (d: Double) => mdlModel + d < lastBestMDL)
      println(s"ub = $ub")
      assert(ub <= lastBestScore)
      bestModel.solver.add(bestModel.totalCost <= ub)
    }
    println(bestStats)

    assert(!(baseStats.nSols > 0 && betrStats.nSols > 0) || baseModel.bestCost == betrModel.bestCost)
    assert(!(betrStats.nSols > 0 && bestStats.nSols > 0) || betrModel.bestCost == bestModel.bestCost, s"the betr and best cost are not the same ${betrModel.bestCost} != ${bestModel.bestCost}")


    val mdlError = logC(n*n, baseModel.bestCost.get)/log(2)
    val mdl = mdlModel + mdlError
    val results = Seq[(String, Double)]("mdl" -> mdl,
      "model" -> mdlModel, "error" -> mdlError, "cost"->baseModel.bestCost.get,
      "base-nnodes" -> baseStats.nNodes, "base-time" -> baseStats.time,
      "better-nnodes" -> betrStats.nNodes, "better-time" -> betrStats.time,
      "best-nnodes" -> bestStats.nNodes, "best-time" -> bestStats.time)
    // add the results for this blockmodel
    res append (s"$k\t" + results.map(_._2).mkString("\t") + "\n")

    lastBestScore = math.min(baseModel.bestCost.get, lastBestScore)
    lastBestMDL = math.min(mdl, lastBestMDL)
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
