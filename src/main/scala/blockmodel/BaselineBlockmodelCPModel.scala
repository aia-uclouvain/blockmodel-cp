package blockmodel

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import utils.Digraph

/**
  * baseline CP model for the Block modeling problem. provides:
  * M : the image matrix
  * C : the cluster assignment for each vertex
  * totalCost : the total reconstitution cost of X given the blockmodel descibed by M and C
  * @param graph
  * @param k
  */
class BaselineBlockmodelCPModel(graph: Digraph, k: Int) extends CPModel {
  val n: Int = graph.n // number of nodes in the graph
  val X: Array[Array[Int]] = Array.tabulate(n,n){case (i,j) => if(graph(i,j)) 1 else 0} // adjacency matrix of the graph
  // CP variables:
  val M: Array[Array[CPIntVar]] = Array.fill(k, k)(CPIntVar(0, 1)) // image matrix
  private val flatM: Array[CPIntVar] = M.flatten
  val C: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until k)) // vertex cluster labels

  // CP blockmodel.constraints
  add(gcc(C, values=0 until k, min=1,  max=C.length))

  val totalCost: CPIntVar = sum(n,n)((i, j) => {
    // get the cell of M associated with the cluster of i and the cluster of j
    val M_CiCj = elementVar(flatM, C(i) * k + C(j))
    // the cost is 1 if they are different and 0 otherwise
    absolute(M_CiCj - X(i)(j))
  })

}
