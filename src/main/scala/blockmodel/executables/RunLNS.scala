package blockmodel.executables

import java.io.File

import blockmodel.utils.Matrix._
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import org.rogach.scallop.ScallopConf
import oscar.cp._
import oscar.util.selectMin

import scala.util.Random

/**
  * Executable to run the LNS search procedure. Run with --help for usage information
  */
object RunLNS extends App with BlockmodelSearchResult {
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, argName = "graph file",
      descr = "File describing the graph to process. Data must be given in a text file containing tab " +
        "separated values, with the first line being the name of the vertices and subsequent lines being the " +
        "adjacency matrix where a 1 indicates a tie and a 0 no tie.")
    val k = opt[Int](required = true, argName = "#clusters", default = Some(2), validate = 1<=_,
      descr = "Number of clusters in the resulting block model.")
    val restarts = opt[Int](required = false, default = Some(10), validate = 1<=_,
      descr = "Number of complete restarts to perform")
    val nbFail = opt[Int](required = false, default = Some(1000), validate = 1 <= _,
      descr = "Max number of failed nodes before aborting run")
    val failedRuns = opt[Int](required = false, default = Some(100), validate = 1 <= _,
      descr = "Max number of consecutive failed runs before a restart")
    val alpha = opt[Double](required = false, default = Some(0.05), validate = 0<_,
      descr = "Initial relaxation factor alpha. This is the initial fraction of vertices being relaxed")
    val adaptative = opt[Boolean](required = false, descr = "if this is set, the alpha value will be changed " +
      "dynamically during the search")
    val seed = opt[Int](required = false,
      descr = "Seed for the random number generator")
    val output = opt[String](required = true, descr = "File in which statistics about the search will be written, in " +
      "JSON format.")
    val image = opt[Boolean](required = false, descr = "If this is set, an image of the resulting block model will" +
      " be generated, and saved to [output].gif")
    val time = opt[Int](required = false, default = Some(60), descr = "Time budget for the solver in seconds.",
      validate = 1<=_)
    val verbose = opt[Boolean](required = false, default = Some(false),
      descr = "If set, gives progress update during the search")
    verify()
  }

  val conf = new Conf(args)  // Note: This line also works for "object Main extends App"

  // BlockmodelSearchResult variables
  var getName = ""
  var getDescripton = ""
  var getGraphFile = conf.in()
  var getK = conf.k()
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

  val startTime = System.currentTimeMillis()

  val g = Digraph.fromTSV(getGraphFile)
  if (verbose) println(s"loaded graph $getGraphFile with ${g.n} nodes and ${g.e} edges")

  class Model extends BlockmodelCPModel(g, getK) {
    var bestBM: Option[Blockmodel] = None
    minimize(totalCost) search {
      if (! C.areBound) symmetryBreakingBranchingIdx(C, minDom(C), blockmodelConstraint.delta)
      else M.flatten.find(!_.isBound) match {
        case Some(x) => branch(add(x === x.getMin))(add(x !== x.getMin))
        case None => noAlternative
      }
    }
    onSolution {
      bestBM = Some(getBlockmodel)
      if (getSolution.isEmpty || getScoreOfSolutions.last > totalCost.value) {
        val time = System.currentTimeMillis() - startTime
        getTimeOfSolutions :+= time
        getScoreOfSolutions :+= totalCost.value
        val blockmodel = getBlockmodel
        getSolution = Some(blockmodel)
        if (verbose) {
          println("***")
          println(blockmodel)
          //println(cost.toStringMatrix)
          println("totalcost is " + totalCost + " (" + (totalCost.value*100/n/n) + "%)")
        }
      }
    }
    if (!verbose) solver.silent = true
  }

  // LNS search procedure
  var nbFail = conf.nbFail()
  var α = conf.alpha()
  val random = conf.seed.toOption match {
    case Some(i) => new Random(i)
    case None => new Random()
  }
  val maxFailedRuns = conf.failedRuns()
  val maxNbRestarts = conf.restarts()

  private def getTimeLeft: Int = (getTimeBudget - ((System.currentTimeMillis() - startTime)/1000)).toInt

  var nbRestarts = 0
  while (getTimeLeft > 0 && nbRestarts < maxNbRestarts) {
    nbRestarts += 1
    val model = new Model()

    if (verbose) println("looking for initial solution")
    val stats = model.solver.startSubjectTo(nSols = 1, timeLimit = getTimeLeft) {
      /*model.solver.search(
        /*
        conflictOrderingSearch(model.C, model.blockmodelConstraint.minCostDelta(_) + random.nextFloat(), model.blockmodelConstraint.smallestCostDelta(_)) ++
          binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
         */
        binaryFirstFail(model.C) ++ binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))

      )*/
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
    while (getTimeLeft > 0 && runsSinceLastSolution < maxFailedRuns) {
      runNb += 1
      runsSinceLastSolution += 1
      val bestCost = getSolution.get.cost(g)
      val localBestCost = localBest.cost(g)
      if (verbose) println(s"restart $nbRestarts/$maxNbRestarts, $runsSinceLastSolution/$maxFailedRuns failed runs, best:$localBestCost (global:$bestCost), ${getTimeLeft / 60}mins left, relaxing ${(α * 100).toInt}%, max $nbFail failed nodes")
      // run using the max Cost Delta heuristic and a limit on the number of failed states
      // randomly relaxing α% of the variables
      val stats = model.solver.startSubjectTo(failureLimit = nbFail, timeLimit = getTimeLeft) {
        /*model.solver.search(
          /*
          conflictOrderingSearch(model.C, model.blockmodelConstraint.sumCostDelta(_) + random.nextFloat(), model.blockmodelConstraint.smallestCostDelta(_)) ++
            binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
           */
          binaryFirstFail(model.C) ++ binaryIdx(model.M.flatten, model.blockmodelConstraint.MVarHeuris(_), model.blockmodelConstraint.MValHeuris(_))
        )*/
        model.solver.add(model.totalCost < localBestCost)
        // relax randomly α% of the C variables
        val bestC = localBest.C
        //model.solver.add((0 until g.n).filter(i => random.nextFloat() > α).map(i => model.C(i) === bestC(i)))

        /*
        val costs = localBest.costOfVertices(g)
        val total = costs.sum
        val selec = (0 until g.n).filter(i => random.nextFloat() > 0.1 * α + 0.9 * α * g.n * costs(i).toDouble / total)
        model.solver.add(selec.map(i => model.C(i) === bestC(i)))
         */

        val selec = (0 until g.n).filter(i => random.nextFloat() > (0.1*α)+(0.9*α*g.n*costOfVertex(i,localBest.C,localBest.M)/localBestCost/2))
        model.solver.add(selec.map(i => model.C(i) === bestC(i)))
      }
      // if a solution is found, print the stats
      if (stats.nSols > 0) {
        runsSinceLastSolution = 0
        localBest = model.bestBM.get
        if (verbose) println(stats)
      }
      if(conf.adaptative.getOrElse(false)) {
        if (stats.completed) α = math.min(1.0, α * 1.21)
        else α = math.max(conf.alpha(), α / 1.1)
      }
    }
    if (verbose) println("restarting")
  }

  getSolution match {
    case Some(s) => {
      println("Solution found:")
      println(s)

      getTimeToBest = getTimeOfSolutions.last
      getScoreOfBest = getScoreOfSolutions.last
      println(s"Cost of the solution: $getScoreOfBest")

      if (g.n < 30) println(s.toStringGrouped(g))
      if (conf.image.getOrElse(false)) {
        println("generating image")
        ImageIO.write(s.toImageGrouped(g), "gif", new File("./out.gif"))
      }
    }
    case None => println("No solution found.")
  }


  conf.output.toOption match {
    case Some(value) => {
      getName = value
      storeJson()
    }
    case _ => {}
  }

  def symmetryBreakingBranchingIdx(variables: Array[CPIntVar], varHeuristic: Int => Int, valScore: (Int, Int) => Int)(implicit cp: CPSolver): Seq[Alternative] = {
    // index of the unbound variable which is best according to varHeuristic
    val bestIdx = selectMin(variables.indices)(!variables(_).isBound)(varHeuristic).get
    val (used, unused) = variables(bestIdx).partition(value => variables.exists(_.isBoundTo(value)))
    val allowedValues = if (unused.isEmpty) used.toIndexedSeq else used.toIndexedSeq :+ unused.head
    val orderedValues = allowedValues.sortBy(valScore(bestIdx, _))
    branchAll(orderedValues)(v => add(variables(bestIdx) === v))
  }

  def costOfVertex(v: Int, C: Array[Int], B: Array[Array[Boolean]]): Int = {
    var sum = 0
    var i = 0
    while (i < g.n) {
      if(g(v)(i) != B(C(v))(C(i))) sum += 1
      if(g(i)(v) != B(C(i))(C(v))) sum += 1
      i += 1
    }
    sum
  }


}
