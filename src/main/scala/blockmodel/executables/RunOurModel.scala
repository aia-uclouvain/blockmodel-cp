package blockmodel.executables

import java.io.File

import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.Matrix._
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel}
import javax.imageio.ImageIO
import javax.imageio.stream.FileImageOutputStream
import org.rogach.scallop.ScallopConf
import oscar.cp._

import scala.concurrent.duration.{Duration, MINUTES}

object RunOurModel extends App with BlockmodelSearchResult {

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
    val ub = opt[Int](required = false, descr = "upper bound")
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
  object model extends BlockmodelCPModel(g, getK) {
    // for (i <- 0 until getK) add(C(i) <= i)
    //for (i <- 0 until n) add(C(i) <= maximum(C.drop(i)))
    val decisionVars = C ++ M.flatten
    minimize(totalCost)

    if (conf.ub.isDefined)
      add(totalCost <= conf.ub.getOrElse(n*n))

    //val helper = new WeightedDegreeHelper(decisionVars.head.store, decisionVars, 0.99)
    search(
      //PermutationBreakingBranching(C, blockmodelConstraint.biggestCostDelta, identity) ++ binaryMaxWeightedDegree(M.flatten.asInstanceOf[Array[CPIntVar]])
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
