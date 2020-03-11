package blockmodel.utils

import blockmodel.Blockmodel

/**
  * common interface for methods that perform an exhaustive search for the best blockmodel
  */
trait BlockmodelSearchResult {
  def getName: String // name of the method used for the search
  def getDescripton: String // description of the search method
  def getGraphFile: String // path to the file containing the graph to be clustered
  def getK: Int // number of clusters
  def getTimeOfSolutions: Array[Long] // time at which intermediary solutions where found
  def getScoreOfSolutions: Array[Float] // scores of the intermediary solutions
  def getTimeToComplete: Long // time until the search was completed (i.e. the optimality was proven)
  def getTimeToBest: Long
  def getScoreOfBest: Float
  def getNNodes: Long // number of search nodes explored during the search
  def isCompleted: Boolean // was the search complete? i.e. was optimality proven?
  def getTimeBudget: Long
  def getSolution: Option[Blockmodel]

  def storeJson(): Unit = {
    val res: JsonBuilder = new JsonBuilder()
    res.add("name", getName)
    res.add("description", getDescripton)
    res.add("graphFile", getGraphFile)
    res.add("k", getK)
    res.add("times", getTimeOfSolutions)
    res.add("scores", getScoreOfSolutions)
    res.add("bestScore", getScoreOfBest)
    res.add("timeToBest", getTimeToBest)
    res.add("nNodes", getNNodes)
    res.add("completed", isCompleted.toString)
    res.add("timeToCompleted", getTimeToComplete)
    res.add("timeBudget", getTimeBudget)
    res.add("solution", getSolution.map(_.toString).getOrElse("no solution found."))
    new FileReporter(res.toString(), getName + ".JSON")
  }
}
