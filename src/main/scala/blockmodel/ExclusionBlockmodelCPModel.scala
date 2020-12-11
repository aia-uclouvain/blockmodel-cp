package blockmodel

import java.io.File

import blockmodel.constraints.{BlockmodelCost, BlockmodelCostPartial}
import blockmodel.utils._
import oscar.cp._

class ExclusionBlockmodelCPModel(graph: Digraph, val k: Int) extends CPModel {
  val n: Int = graph.n // number of nodes in the graph
  val X: Array[Array[Boolean]] = graph.adjacencyMatrix // adjacency matrix of the graph
  // CP variables:
  val M: Array[Array[CPBoolVar]] = Array.fill(k+1,k+1)(CPBoolVar()) // image matrix
  for(i <- 0 until k+1) {
    M(i)(k).assignFalse()
    M(k)(i).assignFalse()
  }
  val C: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until k+1)) // vertex cluster labels
  val cost: Array[Array[CPIntVar]] = Array.tabulate(k+1,k+1){
    case (i,j) => if(i < k && j < k) CPIntVar(0) else CPIntVar(0 until n*n)
  }
  val totalCost: CPIntVar = CPIntVar(0 to n*n) // total cost of the block model
  // CP blockmodel.constraints
  add(gcc(C, values=0 until k, min=1,  max=C.length))
  add(sum(cost.flatten, totalCost)) // totalCost = sum of all costs
  val blockmodelConstraint = new BlockmodelCost(X,M,C,cost,totalCost)
  add(blockmodelConstraint)
  val nbExcluded = CPIntVar(0 to n)
  add(countEq(nbExcluded, C, k))

  for(i <- 0 until n; j <- 0 until n){
    if ((0 until n).forall(a => X(i)(a) == X(j)(a) && X(a)(i) == X(a)(j)))
      add(C(i) === C(j))
  }

  // first vertex always in last position or excluded
  add(C(0) >= k-1)
  // clusters always in ascending order
  for (i <- 1 until n) add(C(i) >= minimum(C.take(i))-1, Strong)



  /**
    * @return the blockmodel found by the search. fails if the search was not completed.
    */
  def getBlockmodel: Blockmodel = new Blockmodel(C.map(_.value), M.map(_.map(_.isTrue)))
}

class ExclusionBlockmodelCPModel2(graph: Digraph, val k: Int) extends CPModel {
  val n: Int = graph.n // number of nodes in the graph
  val X: Array[Array[Boolean]] = graph.adjacencyMatrix // adjacency matrix of the graph
  // CP variables:
  val M: Array[Array[CPBoolVar]] = Array.fill(k,k)(CPBoolVar()) // image matrix
  val C: Array[CPIntVar] = Array.fill(n)(CPIntVar(0 until k+1)) // vertex cluster labels
  val cost: Array[Array[CPIntVar]] = Array.fill(k,k)(CPIntVar(0))
  val totalCost: CPIntVar = CPIntVar(0) // total cost of the block model
  // CP blockmodel.constraints
  add(gcc(C, values=0 until k, min=1,  max=C.length))
  val blockmodelConstraint = new BlockmodelCostPartial(X,M,C,cost,totalCost)
  add(blockmodelConstraint)
  val nbExcluded = CPIntVar(0 to n)
  add(countEq(nbExcluded, C, k))

  for(i <- 0 until n; j <- 0 until n){
    if ((0 until n).forall(a => X(i)(a) == X(j)(a) && X(a)(i) == X(a)(j)))
      add(C(i) === C(j))
  }

  // first vertex always in last position or excluded
  add(C(0) >= k-1)
  // clusters always in ascending order
  for (i <- 1 until n) add(C(i) >= minimum(C.take(i))-1, Strong)



  /**
    * @return the blockmodel found by the search. fails if the search was not completed.
    */
  def getBlockmodel: Blockmodel = {
    val c = C.map(_.value)
    if (c.max < k) new Blockmodel(C.map(_.value), M.map(_.map(_.isTrue)))
    else {
      val m = Array.tabulate(c.max+1, c.max+1){(i,j) =>
        if (i < k && j < k) M(i)(j).isTrue
        else false
      }
      new Blockmodel(c,m)
    }
  }
}

object compare extends App {
  import scala.util.Random
  import javax.imageio.ImageIO

  val k = 4
  val n = 30
  val rng = new Random(0)
  val error = 0.05

  val maxNbExcluded = 14

  val (g, bm) = Digraph.randomWithImage(Blockmodel.ringStructure(k), n, rng, error)
  val img = bm.toImageGrouped(g, 10)

  var sols = Seq[Blockmodel]()
  // model with k+1 clusters. the last cluster, with index k, is the excluded variables.
  object model1 extends ExclusionBlockmodelCPModel(g, k) {
    add(nbExcluded <= maxNbExcluded)
    search(binaryFirstFail(C) ++ binary(M.flatten.asInstanceOf[Array[CPIntVar]]))
    onSolution {
      val bm = getBlockmodel
      //println(bm.toStringGrouped(g))
      sols :+= bm
    }
  }

  println("*** solving with old model")
  val s1 = model1.solver.start()
  println(s1)

  var sols2 = Seq[Blockmodel]()
  object model2 extends ExclusionBlockmodelCPModel2(g, k) {
    add(nbExcluded <= maxNbExcluded)
    search(binaryFirstFail(C) ++ binary(M.flatten.asInstanceOf[Array[CPIntVar]]))
    onSolution {
      val bm = getBlockmodel
      //println(bm.toStringGrouped(g))
      sols2 :+= bm
    }
  }

  println("*** solving with new model")
  val s2 = model2.solver.start()
  println(s2)

  val trustedSolutions = sols.toSet
  val otherSolutions = sols2.toSet

  val falseSolutions = otherSolutions &~ trustedSolutions
  val unfoundSolutions = trustedSolutions &~ otherSolutions

  assert(falseSolutions.isEmpty, s"there were ${falseSolutions.size} false solutions.")
  assert(unfoundSolutions.isEmpty)

  assert(trustedSolutions == otherSolutions)
}