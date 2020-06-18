package blockmodel.executables

import java.io.File

import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.Matrix._
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import org.rogach.scallop.ScallopConf
import oscar.cp._
import oscar.cp.searches.WeightedDegreeHelper

object RunCPModel extends App with BlockmodelSearchResult {
  object SearchProcedure extends Enumeration {
    val BINARY, BINARY_LAST_CONFLICT, CONFLICT, DYNAMIC_SYMMETRY_BREAKING = Value
    def contains(s: String): Boolean = values.exists(_.toString == s)
  }
  object VarHeuris extends Enumeration {
    val FIXED, MINDOM, WDEG, HEURISTIC = Value
    def contains(s: String): Boolean = values.exists(_.toString == s)
  }

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, argName = "graph file",
      descr = "File describing the graph to process. Data must be given in a text file containing tab " +
        "separated values, with the first line being the name of the vertices and subsequent lines being the " +
        "adjacency matrix where a 1 indicates a tie and a 0 no tie.")
    val k = opt[Int](required = true, argName = "#clusters", default = Some(2), validate = 1<=_,
      descr = "Number of clusters in the resulting block model.")
    val searchProc = opt[String](required = false, default = Some(SearchProcedure.BINARY.toString),
      descr = s"Search procedure for the CP solver, amongst ${SearchProcedure.values.mkString(", ")}.",
      validate = SearchProcedure.contains).map(SearchProcedure.withName)
    val varHeuris = opt[String](required = false, default = Some(VarHeuris.WDEG.toString),
      descr = s"Variable ordering heuristic for the CP solver, amongst ${VarHeuris.values.mkString(", ")}.",
      validate = VarHeuris.contains).map(VarHeuris.withName)
    val ub = opt[Int](required = false, descr = "Optional upper bound on the cost of the block model. If specified, " +
      "the search will only return block models with cost <= to it, if any exist.")
    val output = opt[String](required = true, descr = "File in which statistics about the search will be written, in " +
      "JSON format.")
    val visual = opt[Boolean](required = false, descr = "If this is set, an image of the resulting block model will" +
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

  val verbose = conf.verbose()
  if (verbose) println(conf.varHeuris)

  val startTime = System.currentTimeMillis()

  val g = Digraph.fromTSV(getGraphFile)
  object model extends BlockmodelCPModel(g, getK) {
    minimize(totalCost)

    if (conf.ub.isDefined)
      add(totalCost <= conf.ub())

    search {
      // the search for M is always the same: binary branching, using the heuristic defined in the blockmodel
      // constraint, which will first put the values that minimize the cost
      val MSearch  = binaryIdx(M.flatten, blockmodelConstraint.MVarHeuris(_), blockmodelConstraint.MValHeuris(_))
      // decisionVars groups all the decision variables in one array
      val decisionVars: Array[CPIntVar] = C ++ M.flatten
      import SearchProcedure._
      import VarHeuris._
      val b: oscar.algo.search.Branching = conf.searchProc() match {
        case BINARY => conf.varHeuris() match {
          case FIXED => binary(decisionVars)
          case MINDOM => binaryFirstFail(decisionVars)
          case WDEG => binaryMaxWeightedDegree(decisionVars)
          case HEURISTIC => binaryIdx[Int](C, -blockmodelConstraint.sumCostDelta(_),
            blockmodelConstraint.smallestCostDelta(_)) ++ MSearch
          case _ => binaryFirstFail(decisionVars)
        }
        case BINARY_LAST_CONFLICT => conf.varHeuris() match {
          case FIXED => binaryLastConflict(decisionVars, identity, minVal(decisionVars))
          case MINDOM => binaryLastConflict(decisionVars, minDom(decisionVars), minVal(decisionVars))
          case WDEG => {
            val helper = new WeightedDegreeHelper(decisionVars.head.store, decisionVars, 0.99)
            binaryLastConflict(decisionVars, i => -(helper.getWeightedDegree(decisionVars(i)) * 1000).round.toInt, minVal(decisionVars))
          }
          case _ => binaryLastConflict(decisionVars)
        }
        case CONFLICT => conf.varHeuris() match {
          case FIXED => conflictOrderingSearch(decisionVars, identity, minVal(decisionVars))
          case MINDOM => conflictOrderingSearch(decisionVars, minDom(decisionVars), minVal(decisionVars))
          case WDEG => {
            val helper = new WeightedDegreeHelper(decisionVars.head.store, decisionVars, 0.99)
            conflictOrderingSearch(decisionVars, i => -(helper.getWeightedDegree(decisionVars(i)) * 1000).round.toInt,
              minVal(decisionVars))
          }
          case _ => conflictOrderingSearch(decisionVars, minDom(decisionVars), minVal(decisionVars))
        }
        case DYNAMIC_SYMMETRY_BREAKING => conf.varHeuris() match {
          case FIXED => PermutationBreakingBranching(C, identity, (_,_) => 0) ++ MSearch
          case MINDOM => PermutationBreakingBranching(C, minDom(C), (_,_) => 0) ++ MSearch
          case WDEG => {
            val helper = new WeightedDegreeHelper(decisionVars.head.store, decisionVars, 0.99)
            PermutationBreakingBranching(decisionVars,
              i => -(helper.getWeightedDegree(decisionVars(i)) * 1000).round.toInt, (_,_) => 0)
          }
          case HEURISTIC => PermutationBreakingBranching(C, i => -blockmodelConstraint.sumCostDelta(i),
            blockmodelConstraint.delta(_, _)) ++ MSearch
          case _ => PermutationBreakingBranching(C, minDom(C), (_,_) => 0) ++ MSearch
        }
        case _ => {
          if (verbose) println("no special heuristics given, doing binary max weighted degree")
          binaryMaxWeightedDegree(decisionVars)
        }
      }

      b
    }
    onSolution {
      // store the time and score for statistics
      val time = System.currentTimeMillis() - startTime
      getTimeOfSolutions :+= time
      getScoreOfSolutions :+= totalCost.value
      val blockmodel = getBlockmodel
      getSolution = Some(blockmodel)
      if (verbose) {
        println("***")
        println(blockmodel)
        println(cost.toStringMatrix)
        println("totalcost is " + totalCost)
      }
    }
  }
  // run the solver
  val stat = model.solver.startSubjectTo(timeLimit = getTimeBudget.toInt) {}
  if (verbose) println(stat)
  getSolution match {
    case Some(s) => {
      println("Solution found:")
      println(s)
      if (conf.visual.getOrElse(false)) {
        println(s"generating image ${conf.output()}.gif")
        ImageIO.write(s.toImageGrouped(g), "gif", new File(conf.output()+".gif"))
      }
    }
    case None => println("No solution found.")
  }
  isCompleted = stat.completed
  if (isCompleted) {
    val time = System.currentTimeMillis() - startTime
    getTimeToComplete = time
  }

  getNNodes = stat.nNodes
  getTimeToBest = getTimeOfSolutions.last
  getScoreOfBest = getScoreOfSolutions.last
  println(s"Cost of solution: $getScoreOfBest")

  conf.output.toOption match {
    case Some(value) => {
      getName = value
      storeJson()
    }
    case _ => {}
  }


}
