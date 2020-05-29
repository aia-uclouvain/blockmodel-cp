package blockmodel.executables

import java.io.File

import blockmodel.search.{PermutationBreakingBranching, VertexDistanceHeuristic}
import blockmodel.utils.Matrix._
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import org.rogach.scallop.ScallopConf
import oscar.cp._
import oscar.cp.searches.WeightedDegreeHelper

import scala.util.Random

object RunLNS extends App with BlockmodelSearchResult {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, argName = "graph file")
    val k = opt[Int](required = true, argName = "#clusters", default = Some(2), validate = 1<=_)
    val restarts = opt[Int](required = false, descr = "number of restarts", default = Some(10), validate = 1<=_)
    val nbFail = opt[Int](required = false, descr = "max number of failed nodes before aborting run",
      default = Some(1000), validate = 1 <= _)
    val failedRuns = opt[Int](required = false, descr = "max number of consecutive failed runs before a restart",
      default = Some(100), validate = 1 <= _)
    val goodFirstSol = opt[Boolean](required = false)
    val seed = opt[Int](required = false, descr = "seed for the random number generator")
    val output = opt[String](required = true, descr = "output")
    val visual = opt[Boolean](required = false)
    val description = opt[String](required = false, descr = "description")
    val time = opt[Int](required = false, default = Some(60), descr = "time budget for the solver (in seconds)", validate = 1<=_)
    val verbose = opt[Boolean](required = false, default = Some(false), descr = "verbose")
    verify()
  }

  val conf = new Conf(args)  // Note: This line also works for "object Main extends App"

  // BlockmodelSearchResult variables
  var getName = ""
  var getDescripton = ""
  var getGraphFile = ""
  var getK = -1
  var getTimeOfSolutions = Array()
  var getScoreOfSolutions = Array()
  var getTimeToComplete = -1L
  var getTimeToBest = -1L
  var getScoreOfBest = -1.0f
  var getNNodes = -1L
  var isCompleted = false
  var getTimeBudget = -1L
  var getSolution: Option[Blockmodel] = None

  val verbose = conf.verbose.getOrElse(false)

  val startTime = System.currentTimeMillis()
  getGraphFile = conf.in()
  getK = conf.k()
  //getName = config.output.getOrElse("")
  getTimeBudget = conf.time()
  getDescripton = conf.description.getOrElse("")

  val g = Digraph.fromTSV(getGraphFile)
  if (verbose) println(s"loaded graph $getGraphFile with ${g.n} nodes and ${g.e} edges")

  class Model extends BlockmodelCPModel(g, getK) {
    var bestBM: Option[Blockmodel] = None
    minimize(totalCost)
    onSolution {
      bestBM = Some(getBlockmodel())
      if (getSolution.isEmpty || getSolution.get.cost(g) > totalCost.value) {
        val time = System.currentTimeMillis() - startTime
        getTimeOfSolutions :+= time
        getScoreOfSolutions :+= totalCost.value
        val blockmodel = getBlockmodel()
        getSolution = Some(blockmodel)
        if (verbose) {
          println("***")
          println(blockmodel)
          println(cost.toStringMatrix)
          println("totalcost is " + totalCost)
        }
      }
    }
    if (!verbose) solver.silent = true
  }

  // LNS search procedure

  var nbFail = conf.nbFail()
  var α = 0.05
  val random = conf.seed.toOption match {
    case Some(i) => new Random(i)
    case None => new Random()
  }
  val maxFailedRuns = conf.failedRuns()
  val maxNbRestarts = conf.restarts()

  def getTimeLeft(): Int = (getTimeBudget - ((System.currentTimeMillis() - startTime)/1000)).toInt


  var nbRestarts = 0
  while (getTimeLeft() > 0 && nbRestarts < maxNbRestarts) {
    nbRestarts += 1
    val model = new Model()

    if (verbose) println("looking for initial solution")
    val stats = model.solver.startSubjectTo(nSols = 1, timeLimit = getTimeLeft()) {
      if (conf.goodFirstSol.getOrElse(false)) {
        model.solver.search({
          val h = new VertexDistanceHeuristic(model.C, model.k, g)
          binaryIdx[Double](model.C, i => h.vertexMinDist(i) + random.nextDouble(), h.bestCluster(_)) ++
            binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
        })
      } else {
        model.solver.search(
          conflictOrderingSearch(model.C, model.blockmodelConstraint.minCostDelta(_) + random.nextFloat(), model.blockmodelConstraint.smallestCostDelta(_)) ++
            binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
        )
      }
    }

    if (stats.nSols == 0) {
      println("no initial solution found...")
      println(stats)
      System.exit(0)
    }
    var localBest = model.bestBM.get

    // try to improve on this initial solution
    var runNb = 0
    var runsSinceLastSolution = 0
    while (getTimeLeft() > 0 && runsSinceLastSolution < maxFailedRuns) {
      runNb += 1
      runsSinceLastSolution += 1
      val bestCost = getSolution.get.cost(g)
      val localBestCost = localBest.cost(g)
      if (verbose) println(s"restart $nbRestarts/$maxNbRestarts, $runsSinceLastSolution/$maxFailedRuns failed runs, best:$localBestCost (global:$bestCost), ${getTimeLeft() / 60}mins left, relaxing ${(α * 100).toInt}%, max $nbFail failed nodes")
      // run using the max Cost Delta heuristic and a limit on the number of failed states
      // randomly relaxing α% of the variables
      val stats = model.solver.startSubjectTo(failureLimit = nbFail, timeLimit = getTimeLeft()) {
        model.solver.search(
          conflictOrderingSearch(model.C, model.blockmodelConstraint.sumCostDelta(_)+random.nextFloat(), model.blockmodelConstraint.smallestCostDelta(_)) ++
            binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
        )
        model.solver.add(model.totalCost < localBestCost)
        // relax randomly α% of the C variables
        val bestC = localBest.C
        model.solver.add((0 until g.n).filter(i => random.nextFloat() > α).map(i => model.C(i) === bestC(i)))
      }
      // if a solution is found, print the stats
      if (stats.nSols > 0) {
        runsSinceLastSolution = 0
        localBest = model.bestBM.get
        if (verbose) println(stats)
      }
      if (stats.completed) α = math.min(1.0, α*1.1)
      else α = math.max(0.01, α/1.1)
    }
    if (verbose) println("restarting")
  }

  getSolution.foreach(s => {
    println(s)

    getTimeToBest = getTimeOfSolutions.last
    getScoreOfBest = getScoreOfSolutions.last
    println(getScoreOfBest)

    if (g.n < 30) println(s.toStringGrouped(g))
    if (conf.visual.getOrElse(false)) {
      println("generating image")
      ImageIO.write(s.toImageGrouped(g), "gif", new File("./out.gif"))
    }
  })


  conf.output.toOption match {
    case Some(value) => {
      getName = value
      storeJson()
    }
    case _ => {}
  }


}
