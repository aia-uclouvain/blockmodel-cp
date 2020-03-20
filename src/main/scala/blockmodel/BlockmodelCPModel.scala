package blockmodel

import constraints.BlockmodelCost
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import utils._

/**
  * CP model for the blockmodel problem using the BlockmodelCost global constraint
  */
class BlockmodelCPModel(graph: Digraph, val k: Int) extends CPModel {
  val n: Int = graph.n // number of nodes in the graph
  val X: Array[Array[Boolean]] = graph.adjacencyMatrix // adjacency matrix of the graph
  // CP variables:
  val M: Array[Array[CPBoolVar]] = Array.fill(k,k)(CPBoolVar()) // image matrix
  val C: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until k)) // vertex cluster labels
  val cost: Array[Array[CPIntVar]] = Array.fill(k,k)(CPIntVar(0 to n*n)) // cost of each block
  val totalCost: CPIntVar = CPIntVar(0 to n*n) // total cost of the block model
  // CP blockmodel.constraints
  add(gcc(C, values=0 until k, min=1,  max=C.length))
  add(sum(cost.flatten, totalCost)) // totalCost = sum of all costs
  val blockmodelConstraint = new BlockmodelCost(X,M,C,cost,totalCost)
  add(blockmodelConstraint)

  if (graph.isSymmetrical) {
    println("symmetrical :-) we can do some nice stuff here")
    for (i <- 0 until k; j <- 0 until i){
      add(M(i)(j) === M(j)(i))
      add(cost(i)(j) === cost(j)(i))
    }
  }
}