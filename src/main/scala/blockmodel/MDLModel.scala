package blockmodel

import blockmodel.constraints._
import blockmodel.utils._
import blockmodel.utils.DoubleVar._
import oscar.cp._
import blockmodel.constraints.LogBinomialConstraint.logBinomial

class MDLModel(graph: Digraph, maxK: Int) extends CPModel {
  val n: Int = graph.n // number of nodes in the graph
  val X: Array[Array[Boolean]] = graph.adjacencyMatrix // adjacency matrix of the graph
  // CP variables:
  val M: Array[Array[CPBoolVar]] = Array.fill(maxK,maxK)(CPBoolVar()) // image matrix
  val C: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until maxK)) // vertex cluster labels
  val k: CPIntVar = maximum(C)
  val cost: Array[Array[CPIntVar]] = Array.fill(maxK,maxK)(CPIntVar(0 to n*n/2)) // cost of each block
  val totalCost: CPIntVar = CPIntVar(0 to n*n/2) // total cost of the block model
  // CP blockmodel.constraints
  add(sum(cost.flatten, totalCost)) // totalCost = sum of all costs
  val blockmodelConstraint = new BlockmodelCost(X,M,C,cost,totalCost)
  add(blockmodelConstraint)

  val mdl = DoubleVar(0.0, n*n*math.log(2))
  // log(k)*n + k^2 + log(C(n^2, totalCost)) = mdl
  //add(new SumDouble(Array( log(k)*n, mul(k,k): DoubleVar, logBinomial(n*n, totalCost)), mdl))
}