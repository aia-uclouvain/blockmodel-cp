package blockmodel.executables

import java.io.File

import blockmodel.{Blockmodel, ShortTableModel}
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import org.rogach.scallop._
import oscar.cp._
import blockmodel.utils.Matrix._

import scala.concurrent.duration.{Duration, MINUTES}

object RunShortTableModel extends App with BlockmodelSearchResult {
  // todo add these as parameters to the command line app
  object Heuristics extends Enumeration {
    val binaryFirstFail, conflictOrderingSearch, dynamicSymmetryBreaking = Value
  }

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val in = opt[String](required = true, descr = "<graph file>")
    val k = opt[Int](required = true, descr = "#clusters")
    val output = opt[String](required = true, descr = "output")
    val description = opt[String](required = false, descr = "description")
    val time = opt[Int](required = false, descr = "time(seconds)")
    val verbose = opt[Boolean](required = false, descr = "verbose")
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


  val startTime = System.currentTimeMillis()
  getGraphFile = conf.in.getOrElse("")
  getK = conf.k.getOrElse(2)
  //getName = config.output.getOrElse("")
  getTimeBudget = conf.time.getOrElse(60)
  getDescripton = conf.description.getOrElse("")

  val g = Digraph.fromTSV(getGraphFile)
  object model extends ShortTableModel(g, getK) {
    for (i <- 0 until getK) add(C(i) <= i)
    //for (i <- 0 until n) add(C(i) <= maximum(C.drop(i)))
    val decisionVars = C ++ M.flatten
    minimize(totalCost)

    //val helper = new WeightedDegreeHelper(decisionVars.head.store, decisionVars, 0.99)
    search(
      binaryMaxWeightedDegree(decisionVars)
      //PermutationBreakingBranching(decisionVars, i => -(helper.getWeightedDegree(decisionVars(i)) * 1000).round.toInt, identity)
      // PermutationBreakingBranching(decisionVars, minDom(decisionVars), identity)
      //conflictOrderingSearch(decisionVars, minDomMaxDegree(decisionVars), minVal(decisionVars))
      //binaryLastConflict(decisionVars)
      //binaryFirstFail(decisionVars)
    )
    onSolution {
      val time = System.currentTimeMillis() - startTime
      getTimeOfSolutions :+= time
      getScoreOfSolutions :+= totalCost.value
      val solC: Array[Int] = C.map(_.value)
      val solM: Array[Array[Boolean]] = M.mapCells(_.value == 1)
      val blockmodel = new Blockmodel(solC, solM)
      getSolution = Some(blockmodel)
      println("***")
      println(getScoreOfSolutions.map(_.toInt).zip(getTimeOfSolutions)
        .map{case (score, time) => s"${time/1000.0}s:$score"}
        .mkString("\t"))
      println(blockmodel)
    }
  }

  val stat = model.solver.startSubjectTo(timeLimit = getTimeBudget.toInt) {}
  println(stat)
  getSolution.foreach(println)
  isCompleted = stat.completed
  if (isCompleted) {
    val time = System.currentTimeMillis() - startTime
    getTimeToComplete = time
  }

  getNNodes = stat.nNodes
  getTimeToBest = getTimeOfSolutions.last
  getScoreOfBest = getScoreOfSolutions.last

  conf.output.toOption match {
    case Some(value) => {
      getName = value
      storeJson()
    }
    case _ => {}
  }


}
