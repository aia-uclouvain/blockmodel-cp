package blockmodel.executables

import java.io.File

import blockmodel.constraints.BinomialConstraint
import blockmodel.executables.RunMDLScoreCurveSynthetic2.n
import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.Matrix._
import blockmodel.utils._
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import oscar.cp._

import scala.math.log
import scala.util.Random

object RunMDLScoreCurveSynthetic2 extends App {
  val maxK = args(0).toInt // max number of clusters for the search
  val n = args(1).toInt // number of nodes in the generated graphs
  val actualK = args(2).toInt // actual number of clusters in the generated graphs
  val noise = args(3).toInt * 0.05 // noise added to the graphs
  val rng = if (args.length > 4) new Random(args(4).toInt) else new Random()

  val logCTable = BinomaialCoefficient.logCTable(n*n)

  val setups = Seq(
    ("ring", Blockmodel.ringStructure(actualK)),
    ("stick", Blockmodel.stickStructure(actualK)),
    ("star", Blockmodel.starStructure(actualK)),
    ("community", Blockmodel.communityStructure(actualK)))

  val trackedValues = Seq("mdl", "model", "error", "cost", "base-nnodes", "base-time", "better-nnodes", "better-time",
    "best-nnodes", "best-time")
  // for every tracked value, we will have maxK entries, so we store then in an array of size maxk
  val stats: Map[String, Array[Seq[Double]]] = trackedValues.map(_ -> Array.fill(maxK)(Seq[Double]())).toMap
  for ((name, blockmodel) <- setups) {
    println("*"*80)
    println(s"processing $name")
    val resultsForThisBlockmodel = new StringBuilder()
    // add the header to the results
    resultsForThisBlockmodel append ("k\t" + trackedValues.mkString("\t") + "\n")
    // get the random digraph for this blockmodel
    val (g, bm) = Digraph.randomWithImage(blockmodel, n, rng, noise)
    g.saveTSV(s"$name-n$n-k$actualK-$noise-graph.tsv")
    ImageIO.write(bm.toImageGrouped(g), "gif", new File(s"$name-n$n-k$actualK-$noise-bm.gif"))
    // some variables to keep track of values
    var lastBestScore = Integer.MAX_VALUE
    var lastBestMDL =   Double.PositiveInfinity
    val logCTable = BinomaialCoefficient.logCTable(n*n)
    // perform the search for every k
    for (k <- 1 to maxK) {
      println("\t***")
      println(s"k = $k")

      val mdlModel = log(n) / log(2) + n * log(k) / log(2) + k * k + log(n * n) / log(2)

      class model extends BlockmodelCPModel(g, k) {
        var bestCost: Option[Int] = None
        var bestBM: Option[Blockmodel] = None
        val decisionVars: Array[CPIntVar] = C ++ M.cells
        minimize(totalCost)
        search {
          PermutationBreakingBranching(C, blockmodelConstraint.maxCostDelta, blockmodelConstraint.delta(_, _)) ++ binaryMaxWeightedDegree(M.flatten.asInstanceOf[Array[CPIntVar]])
        }
        onSolution {
          bestBM = Some(getBlockmodel)
          bestCost = Some(totalCost.value)
        }
        solver.silent = true
      }

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
      val bestStats = bestModel.solver.startSubjectTo() {
        val ub = BinomaialCoefficient.search(logCTable, (d: Double) => mdlModel + d < lastBestMDL)
        println(s"ub = $ub")
        assert(ub <= lastBestScore)
        bestModel.solver.add(bestModel.totalCost <= ub)
      }
      println(bestStats)

      assert(!(baseStats.nSols > 0 && betrStats.nSols > 0) || baseModel.bestCost == betrModel.bestCost)
      assert(!(betrStats.nSols > 0 && bestStats.nSols > 0) || betrModel.bestCost == bestModel.bestCost, s"the betr and best cost are not the same ${betrModel.bestCost} != ${bestModel.bestCost}")

      val mdlError = logC(n * n, baseModel.bestCost.get) / log(2)
      val mdl = mdlModel + mdlError
      println(s"mdl is $mdl")
      val results = Seq[(String, Double)]("mdl" -> mdl,
        "model" -> mdlModel, "error" -> mdlError, "cost"->baseModel.bestCost.get,
        "base-nnodes" -> baseStats.nNodes, "base-time" -> baseStats.time,
        "better-nnodes" -> betrStats.nNodes, "better-time" -> betrStats.time,
        "best-nnodes" -> bestStats.nNodes, "best-time" -> bestStats.time)
      // save the stats for averaging later
      for ((key,value) <- results)  stats(key)(k - 1) :+= value
      // add the results for this blockmodel
      resultsForThisBlockmodel append (s"$k\t" + results.map(_._2).mkString("\t") + "\n")

      lastBestScore = math.min(baseModel.bestCost.get, lastBestScore)
      lastBestMDL = math.min(mdl, lastBestMDL)

      baseModel.bestBM.foreach(m => ImageIO.write(m.toImageGrouped(g), "gif", new File(s"$name-n$n-k$actualK-$noise-mdl-$k.gif")))

    }

    println(resultsForThisBlockmodel.result())
    new FileReporter(resultsForThisBlockmodel.result(), s"$name-n$n-k$actualK-$noise-mdl-curve.dat")
  }

  // save average results
  val averageResults = new StringBuilder()
  averageResults append "k\t" + trackedValues.mkString("\t") + "\n"
  for (k <- 0 until maxK) {
    averageResults append s"${k+1}\t"
    averageResults append trackedValues.map(field => avg(stats(field)(k))).mkString("\t")
    averageResults append "\n"
  }

  println(averageResults.result())
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
