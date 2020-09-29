package blockmodel.executables

import java.io.File

import blockmodel.search.VertexDistanceHeuristic
import blockmodel.utils.Matrix._
import blockmodel.utils.{BinomaialCoefficient, BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import org.rogach.scallop.ScallopConf
import oscar.cp._
import scala.math.log

import scala.util.Random

/*
 * Executable for the MDL LNS search. See usage with --help
 */
object RunMDLLNS extends App with BlockmodelSearchResult {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, argName = "graph file",
      descr = "File describing the graph to process. Data must be given in a text file containing tab " +
      "separated values, with the first line being the name of the vertices and subsequent lines being the " +
      "adjacency matrix where a 1 indicates a tie and a 0 no tie.")
    val kMax = opt[Int](required = true, argName = "max #clusters", default = Some(10), validate = 1<=_,
      descr = s"Maximum number of clusters to consider during the search")
    val restarts = opt[Int](required = false, default = Some(10), validate = 1<=_,
      descr = "Number of complete restarts to perform")
    val nbFail = opt[Int](required = false, default = Some(1000), validate = 1 <= _,
      descr = "Max number of failed nodes before aborting a run")
    val failedRuns = opt[Int](required = false, default = Some(100), validate = 1 <= _,
      descr = "Max number of consecutive failed runs before a restart")
    val alpha = opt[Double](required = false, default = Some(0.05), validate = 0<_,
      descr = "Initial relaxation factor alpha. This is the initial fraction of vertices being relaxed")
    val seed = opt[Int](required = false,
      descr = "Seed for the random number generator")
    val output = opt[String](required = true,
      descr = "File in which statistics about the search will be written, in JSON format.")
    val visual = opt[Boolean](required = false,
      descr = "If this is set, an image of the resulting block model will be generated, and saved to [output].gif")
    val time = opt[Int](required = false, default = Some(60), validate = 1<=_,
      descr = "Time budget for the solver in seconds.")
    val verbose = opt[Boolean](required = false, default = Some(false),
      descr = "If set, gives progress update during the search")
    verify()
  }

  val conf = new Conf(args)  // Note: This line also works for "object Main extends App"

  // BlockmodelSearchResult variables
  var getName = ""
  var getDescripton = ""
  var getGraphFile = conf.in()
  var getK = conf.kMax()
  var getTimeOfSolutions = Array()
  var getScoreOfSolutions = Array()
  var getTimeToComplete = -1L
  var getTimeToBest = -1L
  var getScoreOfBest = -1.0f
  var getNNodes = -1L
  var isCompleted = false
  var getTimeBudget = conf.time()
  var getSolution: Option[Blockmodel] = None

  val verbose = conf.verbose.getOrElse(false)

  val random = conf.seed.toOption match {
    case Some(i) => new Random(i)
    case None => new Random()
  }

  val startTime = System.currentTimeMillis()

  val g = Digraph.fromTSV(getGraphFile)
  if (verbose) println(s"loaded graph $getGraphFile with ${g.n} nodes and ${g.e} edges")

  val logCTable = BinomaialCoefficient.logCTable(g.n*g.n)
  def descriptionLength(bm: Blockmodel): Double = {
    val cost = bm.cost(g)
    val i = math.min(cost, (g.n*g.n)-cost)
    bm.modelDescriptionLength + logCTable(i)
  }
  def modelDescriptionLength(k: Int): Double = {
    log(g.n) / log(2) + g.n * log(k) / log(2) + k * k + log(g.n * g.n) / log(2)
  }

  class Model(k: Int) extends BlockmodelCPModel(g, k) {
    var bestBM: Option[Blockmodel] = None
    minimize(totalCost)
    onSolution {
      val blockmodel = getBlockmodel
      val score = descriptionLength(blockmodel)
      bestBM = Some(blockmodel)
      // if it improves the global best solution, save it.
      if (getSolution.isEmpty || descriptionLength(getSolution.get) > score) {
        val time = System.currentTimeMillis() - startTime
        getTimeOfSolutions :+= time
        getScoreOfSolutions :+= score.toFloat
        getSolution = Some(blockmodel)
        if (verbose) {
          println("***")
          println(blockmodel)
          println(cost.toStringMatrix)
          println("totalcost is " + totalCost)
        }
      }
    }
    //if (!verbose) solver.silent = true
  }

  // utility function to get the number of seconds left to run
  private def getTimeLeft(): Int = (getTimeBudget - ((System.currentTimeMillis() - startTime)/1000)).toInt

  // LNS search procedure
  def lns(initial: Blockmodel, k: Int, nbFail: Int, initialα: Double, maxFailedRuns: Int, maxNbRestarts: Int, random: Random): Blockmodel = {
    val model = new Model(k)
    var α = initialα
    var localBest = initial
    // try to improve on this initial solution
    var runNb = 0
    var runsSinceLastSolution = 0
    while (getTimeLeft() > 0 && runsSinceLastSolution < maxFailedRuns) {
      runNb += 1
      runsSinceLastSolution += 1
      val bestCost = descriptionLength(getSolution.get)
      val localBestCost = descriptionLength(localBest)
      if (verbose && runNb%10==0) println(s"k=$k,$runsSinceLastSolution/$maxFailedRuns failed runs, best:$localBestCost k${localBest.k} (global:$bestCost k${getSolution.get.k}), ${getTimeLeft() / 60}mins left, relaxing ${(α * 100).toInt}%, max $nbFail failed nodes")
      // run using the max Cost Delta heuristic and a limit on the number of failed states
      // randomly relaxing α% of the variables
      val stats = model.solver.startSubjectTo(failureLimit = nbFail, timeLimit = getTimeLeft()) {
        model.solver.search(
          conflictOrderingSearch(model.C, model.blockmodelConstraint.sumCostDelta(_) + random.nextFloat(), model.blockmodelConstraint.smallestCostDelta(_)) ++
            binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
        )
        // relax randomly α% of the C variables
        val bestC = localBest.C
        val costs = localBest.costOfVertices(g)
        val total = costs.sum
        val selec = (0 until g.n).filter(i => random.nextFloat() > 0.1 * α + 0.9 * α * g.n * costs(i).toDouble / total)
        model.solver.add(selec.map(i => model.C(i) === bestC(i)))
      }
      // if a solution is found, save it and print the stats
      if (stats.nSols > 0) {
        runsSinceLastSolution = 0
        localBest = model.bestBM.get
        if (verbose) println(stats)
      }
      if (stats.completed) α = math.min(1.0, α * 1.1)
      else α = math.max(initialα, α / 1.1)
    }
    localBest
  }

  var nbRestarts = 0
  while (getTimeLeft() > 0 && nbRestarts < conf.restarts()) {
    if (verbose) println(s"restart #$nbRestarts")
    nbRestarts += 1
    val model = new Model(2)
    if (verbose) println("looking for initial solution")
    val stats = model.solver.startSubjectTo(nSols = 1, timeLimit = getTimeLeft()) {
      model.solver.search(
        conflictOrderingSearch(model.C, model.blockmodelConstraint.minCostDelta(_) + random.nextFloat(), model.blockmodelConstraint.smallestCostDelta(_)) ++
          binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
      )
    }
    if (stats.nSols == 0) {
      println("no initial solution found...")
      println(stats)
      None
    }

    var lastBest = model.bestBM.get
    var k = 2
    while (k <= conf.kMax() && k <= g.n && modelDescriptionLength(k) < getScoreOfSolutions.last && getTimeLeft() > 0) {
      if (verbose) println(s"running for k=$k")
      val res = lns(initial = lastBest, k, conf.nbFail(), conf.alpha(), conf.failedRuns(), conf.restarts(), random)
      lastBest = res
      k += 1
    }
  }

  getSolution.foreach(s => {
    getTimeToBest = getTimeOfSolutions.last
    getScoreOfBest = getScoreOfSolutions.last
    println(s"best solution found after ${getTimeOfSolutions.last/1000}s, with k=${s.k} clusters:")
    println(s)
    println(s"with a description length of $getScoreOfBest")
    println(s"and a cost of ${s.cost(g)}")

    if (g.n < 30) println(s.toStringGrouped(g))
    if (conf.visual.getOrElse(false)) {
      println(s"generating image ${conf.output()}.gif")
      ImageIO.write(s.toImageGrouped(g), "gif", new File(conf.output()+".gif"))
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
