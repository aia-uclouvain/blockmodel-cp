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

object RunLNS extends App with BlockmodelSearchResult {

  object SearchProcedure extends Enumeration {
    val BINARY, BINARY_LAST_CONFLICT, CONFLICT, DYNAMIC_SYMMETRY_BREAKING = Value
    def contains(s: String): Boolean = values.exists(_.toString == s)
  }
  object VarHeuris extends Enumeration {
    val FIXED, MINDOM, WDEG, TEST = Value
    def contains(s: String): Boolean = values.exists(_.toString == s)
  }

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, argName = "graph file")
    val k = opt[Int](required = true, argName = "#clusters", default = Some(2), validate = 1<=_)
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
  //if (verbose) println(conf.varHeuris)

  val startTime = System.currentTimeMillis()
  getGraphFile = conf.in.getOrElse("")
  getK = conf.k.getOrElse(2)
  //getName = config.output.getOrElse("")
  getTimeBudget = conf.time.getOrElse(60)
  getDescripton = conf.description.getOrElse("")

  val g = Digraph.fromTSV(getGraphFile)
  object model extends BlockmodelCPModel(g, getK) {
    val decisionVars = C ++ M.flatten
    minimize(totalCost)
    val h = new VertexDistanceHeuristic(C, k, g)
    //val hb = binaryIdx[Int](C, h.vertexMinDist, h.bestCluster) ++ binaryLastConflict(M.flatten)
    search {
      binaryFirstFail(decisionVars)
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
