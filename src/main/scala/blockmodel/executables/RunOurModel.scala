package blockmodel.executables

import java.io.File

import blockmodel.utils.Matrix._
import blockmodel.utils.{BlockmodelSearchResult, Digraph}
import blockmodel.{Blockmodel, BlockmodelCPModel, ShortTableModel}
import oscar.cp._
import scopt.OParser

import scala.concurrent.duration.{Duration, MINUTES}

object RunOurModel extends App with BlockmodelSearchResult {
  // todo add these as parameters to the command line app
  object Heuristics extends Enumeration {
    val binaryFirstFail, conflictOrderingSearch, dynamicSymmetryBreaking = Value
  }
  case class Config(
                   graph: File = new File("."),
                   k: Int = -1,
                   timeBudget: Duration = Duration(10, MINUTES),
                   output: Option[String] = None,
                   description: Option[String] = None
  )

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("blockmodel-ourmodel"),
      head("scopt", "4.x"),
      arg[File]("<graph file>")
        .required()
        .action((x, c) => c.copy(graph = x)),
      opt[Int]('k', "clusters")
        .required()
        .action((x, c) => c.copy(k = x))
        .validate(x =>
          if (x > 0) success
          else failure("Number of clusters k must be >0")),
      opt[Duration]('t', "timebudget")
          .optional()
          .action((x,c) => c.copy(timeBudget = x))
          .text("Time budget alloted for the search, e.g. 30s. Default is 10 minutes."),
      opt[String]('o', "output")
        .optional()
        .action((x,c) => c.copy(output = Some(x)))
        .text("output results and search statistics to a json file with the given name"),
      opt[String]('d', "description")
        .optional()
        .action((x,c) => c.copy(output = Some(x)))
        .text("description of the experiment for the JSON file"),
      help("help").text("prints this usage text"),
    )
  }

  // BlockmodelSearchResult variables
  var getName = ""
  var getDescripton = ""
  var getGraphFile = ""
  var getK = -1
  var getTimeOfSolutions = Array()
  var getScoreOfSolutions = Array()
  var getTimeToComplete = -1
  var getTimeToBest = -1
  var getScoreOfBest = -1
  var getNNodes = -1
  var isCompleted = false
  var getTimeBudget = -1
  var getSolution: Option[Blockmodel] = None

  OParser.parse(parser, args, Config()) match {
    case Some(config) => {
      val startTime = System.currentTimeMillis()
      getGraphFile = config.graph.toString
      getK = config.k
      getName = config.output.getOrElse("")
      getTimeBudget = config.timeBudget.toSeconds
      getDescripton = config.description.getOrElse("")

      val g = Digraph.fromTSV(getGraphFile)
      object model extends BlockmodelCPModel(g, getK) {
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

      config.output match {
        case Some(value) => {
          getName = value
          storeJson()
        }
        case _ => {}
      }

    }
    case _ => {
      // arguments are bad, error message will have been displayed
    }
  }


}
