package blockmodel.executables

import java.io.File

import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.Matrix._
import blockmodel.utils.{Digraph, FileReporter}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import oscar.cp._

import scala.math.log
import scala.util.Random

object RunMDLScoreCurveSynthetic extends App {
  val maxK = args(0).toInt // max number of clusters for the search
  val n = args(1).toInt // number of nodes in the generated graphs
  val actualK = args(2).toInt // actual number of clusters in the generated graphs
  val noise = args(3).toInt * 0.05 // noise added to the graphs
  val rng = if (args.length > 4) new Random(args(4).toInt) else new Random()


  val setups = Seq(
    ("ring", Blockmodel.ringStructure(actualK)),
    ("stick", Blockmodel.stickStructure(actualK)),
    ("star", Blockmodel.starStructure(actualK)),
    ("community", Blockmodel.communityStructure(actualK)))

  val stats: Map[String, Array[Seq[Double]]] = Map(
    "mdl" -> Array.fill(maxK)(Seq()),
    "model" -> Array.fill(maxK)(Seq()),
    "error" -> Array.fill(maxK)(Seq()),
    "cost" -> Array.fill(maxK)(Seq())
  )
  for ((name, blockmodel) <- setups) {
    val res = new StringBuilder()
    res append "k\tmdl\tmodel\terror\tcost\n"

    val g = Digraph.randomWithImage(blockmodel, n, rng, noise)
    g.saveTSV(s"$name-n$n-k$actualK-$noise-graph.tsv")

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
      // save the stats for averaging later
      stats("mdl")(k-1) :+= mdl
      stats("model")(k-1) :+= mdl_model
      stats("error")(k-1) :+= mdl_error
      stats("cost")(k-1) :+= model.bestScore
      // add them to the result for this experiment
      res append s"$k\t$mdl\t$mdl_model\t$mdl_error\t${model.bestScore}\n"

      ImageIO.write(model.bestBM.toImageGrouped(g), "gif", new File(s"$name-n$n-k$actualK-$noise-mdl-$k.gif"))
    }

    new FileReporter(res.result(), s"$name-n$n-k$actualK-$noise-mdl-curve.dat")
  }

  // save average results
  val averageResults = new StringBuilder()
  averageResults append "k\tmdl\tmodel\terror\tcost\n"
  for (k <- 0 until maxK) {
    averageResults append s"${k+1}\t"
    averageResults append avg(stats("mdl")(k))
    averageResults append "\t"
    averageResults append avg(stats("model")(k))
    averageResults append "\t"
    averageResults append avg(stats("error")(k))
    averageResults append "\t"
    averageResults append avg(stats("cost")(k))
    averageResults append "\n"
  }

  new FileReporter(averageResults.result(), s"avg-n$n-k$actualK-$noise-mdl-curve.dat")

  def avg(s: Seq[Double]): Double = s.sum / s.length

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
