package blockmodel.constraints

import blockmodel.utils.Digraph
import oscar.cp._
import oscar.cp.core.variables.CPVar
import oscar.cp.core.{CPPropagStrength, Constraint}

class GeneralizedBlockmodel(X: Digraph, C: Array[CPIntVar], block: (Int, Int), st: CPIntVar, nr: CPIntVar, nc: CPIntVar,
                            pr: CPIntVar, pc: CPIntVar, mr: CPIntVar, mc: CPIntVar, sd: CPIntVar, d: CPIntVar)
  extends Constraint(C(0).store, "GeneralizedBlockmodel") {

  override def associatedVars(): Iterable[CPVar] = ???

  override def setup(l: CPPropagStrength): Unit = ???

  val n = X.n
  val (k,l) = block
  val ones = for(i <- 0 until n; j <- 0 until n if X(i,j)) yield (C(i) ?=== k) && (C(j) ?=== l)
  s.add(countEq(st, ones, 1))
}
