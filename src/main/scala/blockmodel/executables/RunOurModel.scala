package blockmodel.executables

import java.io.File

import blockmodel.search.{PermutationBreakingBranching, VertexDistanceHeuristic}
import blockmodel.utils.Matrix._
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import javax.imageio.stream.FileImageOutputStream
import org.rogach.scallop.ScallopConf
import oscar.cp._
import oscar.cp.searches.WeightedDegreeHelper

import scala.concurrent.duration.{Duration, MINUTES}

object RunOurModel extends App with BlockmodelSearchResult {

  object SearchProcedure extends Enumeration {
    val BINARY, BINARY_LAST_CONFLICT, CONFLICT, DYNAMIC_SYMMETRY_BREAKING = Value
    def contains(s: String): Boolean = values.exists(_.toString == s)
  }
  object VarHeuris extends Enumeration {
    val FIXED, MINDOM, WDEG, HEURISTIC, TEST = Value
    def contains(s: String): Boolean = values.exists(_.toString == s)
  }

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, argName = "graph file")
    val k = opt[Int](required = true, argName = "#clusters", default = Some(2), validate = 1<=_)
    val basicSymmetryBreaking = opt[Boolean](required = false, default = Some(false),
      descr = "add constraints to break permutation symmetry by restricting the domain of the first vertex to " +
        "cluster 0, of the second vertex to cluster 0 or 1, etc.")
    val fixedSymmetryBreaking = opt[Boolean](required = false, default = Some(false),
      descr = "add constraints to break permutation symmetry, such that the clusters appear in increasing order")
    val searchProc = opt[String](required = false, default = Some(SearchProcedure.BINARY.toString),
      descr = s"Search procedure, amongst ${SearchProcedure.values.mkString(", ")}.",
      validate = SearchProcedure.contains).map(SearchProcedure.withName)
    val varHeuris = opt[String](required = false, default = Some(VarHeuris.WDEG.toString),
      descr = s"Variable ordering heuristic, amongst ${VarHeuris.values.mkString(", ")}.",
      validate = VarHeuris.contains).map(VarHeuris.withName)
    val ub = opt[Int](required = false, descr = "upper bound")
    val output = opt[String](required = true, descr = "output")
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
  if (verbose) println(conf.varHeuris)

  val startTime = System.currentTimeMillis()
  getGraphFile = conf.in.getOrElse("")
  getK = conf.k.getOrElse(2)
  //getName = config.output.getOrElse("")
  getTimeBudget = conf.time.getOrElse(60)
  getDescripton = conf.description.getOrElse("")

  val g = Digraph.fromTSV(getGraphFile)
  object model extends BlockmodelCPModel(g, getK) {
    // for (i <- 0 until getK) add(C(i) <= i)
    //for (i <- 0 until n) add(C(i) <= maximum(C.drop(i)))
    val decisionVars = C ++ M.flatten
    minimize(totalCost)

    if (conf.ub.isDefined)
      add(totalCost <= conf.ub.getOrElse(n*n))

    search {
      import SearchProcedure._
      import VarHeuris._
      val b: oscar.algo.search.Branching = conf.searchProc() match {
        case BINARY => conf.varHeuris() match {
          case FIXED => binary(decisionVars)
          case MINDOM => binaryFirstFail(decisionVars)
          case WDEG => binaryMaxWeightedDegree(decisionVars)
          case HEURISTIC => binaryIdx[Int](C, blockmodelConstraint.maxCostDelta(_), blockmodelConstraint.smallestCostDelta(_)) ++ binaryIdx(M.flatten, blockmodelConstraint.MVarHeuris(_), blockmodelConstraint.MValHeuris(_))
          case TEST => {
            val h = new VertexDistanceHeuristic(C, k, g)
            binaryIdx[Int](C, h.vertexMaxDist(_), h.bestCluster(_)) ++ binaryIdx(M.flatten, blockmodelConstraint.MVarHeuris(_), blockmodelConstraint.MValHeuris(_))
          }
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
            conflictOrderingSearch(decisionVars, i => -(helper.getWeightedDegree(decisionVars(i)) * 1000).round.toInt, minVal(decisionVars))
          }
          case _ => conflictOrderingSearch(decisionVars, minDom(decisionVars), minVal(decisionVars))
        }
        case DYNAMIC_SYMMETRY_BREAKING => conf.varHeuris() match {
          case FIXED => PermutationBreakingBranching(C, identity, (_,_) => 0) ++ binaryLastConflict(M.flatten)
          case MINDOM => PermutationBreakingBranching(C, minDom(C), (_,_) => 0) ++ binaryLastConflict(M.flatten)
          case WDEG => {
            val helper = new WeightedDegreeHelper(decisionVars.head.store, decisionVars, 0.99)
            PermutationBreakingBranching(decisionVars, i => -(helper.getWeightedDegree(decisionVars(i)) * 1000).round.toInt, (_,_) => 0)
          }
          case HEURISTIC => PermutationBreakingBranching(C, i => -blockmodelConstraint.sumCostDelta(i), blockmodelConstraint.delta(_, _)) ++ binaryIdx(M.flatten, blockmodelConstraint.MVarHeuris(_), blockmodelConstraint.MValHeuris(_))
          case TEST => {
            val h = new VertexDistanceHeuristic(C, k, g)
            PermutationBreakingBranching(C, h.vertexMinDist(_), h.distToCluster(_,_)) ++ binaryIdx(M.flatten, blockmodelConstraint.MVarHeuris(_), blockmodelConstraint.MValHeuris(_))

          }
          case _ => PermutationBreakingBranching(C, minDom(C), (_,_) => 0) ++ binaryLastConflict(M.flatten)
        }
        case _ => {
          if (verbose) println("no special heuristics given, doing binary max weighted degree")
          binaryMaxWeightedDegree(decisionVars)
        }
      }

      b
    }
    onSolution {
      val time = System.currentTimeMillis() - startTime
      getTimeOfSolutions :+= time
      getScoreOfSolutions :+= totalCost.value
      val solC: Array[Int] = C.map(_.value)
      val solM: Array[Array[Boolean]] = M.mapCells(_.value == 1)
      val blockmodel = new Blockmodel(solC, solM)
      getSolution = Some(blockmodel)
      println("***")
      println(blockmodel)
      println(cost.toStringMatrix)
      println("totalcost is " + totalCost)
      println(blockmodel.toStringGrouped(g))
    }
  }

  val stat = model.solver.startSubjectTo(timeLimit = getTimeBudget.toInt) {}
  println(stat)
  getSolution.foreach(s => {
    println(s)
    ImageIO.write(s.toImageGrouped(g), "gif", new File("./out.gif"))
  })
  isCompleted = stat.completed
  if (isCompleted) {
    val time = System.currentTimeMillis() - startTime
    getTimeToComplete = time
  }

  getNNodes = stat.nNodes
  getTimeToBest = getTimeOfSolutions.last
  getScoreOfBest = getScoreOfSolutions.last
  println(getScoreOfBest)

  conf.output.toOption match {
    case Some(value) => {
      getName = value
      storeJson()
    }
    case _ => {}
  }


}
