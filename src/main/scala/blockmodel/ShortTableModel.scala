package blockmodel

import blockmodel.utils._
import oscar.cp._

/**
  * CP model for the blockmodel problem using Short Tables
  */
class ShortTableModel(graph: Digraph, k: Int) extends CPModel {
  val n: Int = graph.n // number of nodes in the graph
  val X: Array[Array[Int]] = Array.tabulate(n,n){case (i,j) => if(graph(i,j)) 1 else 0} // adjacency matrix of the graph
  // CP variables:
  val M: Array[Array[CPIntVar]] = Array.fill(k, k)(CPIntVar(0, 1)) // image matrix
  private val flatM: Array[CPIntVar] = M.flatten
  val C: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until k)) // vertex cluster labels

  // CP blockmodel.constraints
  add(gcc(C, values=0 until k, min=1,  max=C.length))

  val costIJ: Array[Array[CPIntVar]] = Array.fill(n,n)(CPIntVar(0,1))
  private val posI = k*k
  private val posJ = k*k+1
  private val posR = k*k+2

  for (i <- 0 until n; j <- 0 until n) {
    val vars  = flatM :+ C(i) :+ C(j) :+ costIJ(i)(j)
    val table = Array.fill(k*k*2, k*k+3)(-1)
    var a = 0
    for(iVal <- 0 until k; jVal <- 0 until k; mVal <- 0 to 1) {
      table(a)(iVal*k+jVal) = mVal // set the entry coding M( C(i) )( C(j) ) to mVal
      table(a)(posI) = iVal        // set the entry coding C(i) to iVal
      table(a)(posJ) = jVal        // set the entry coding C(j) to jVal
      table(a)(posR) = math.abs(X(i)(j) - mVal) // set the cost entry to the cost
      a += 1
    }
    add(shortTable(vars, table))
  }

  val totalCost: CPIntVar = sum(costIJ.flatten)
}
