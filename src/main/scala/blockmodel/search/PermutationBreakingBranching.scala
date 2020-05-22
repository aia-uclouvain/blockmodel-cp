package blockmodel.search

import oscar.algo.search.{Branching, Decision}
import oscar.cp._

object PermutationBreakingBranching {
  def apply[T](variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: (Int, Int) => Int): PermutationBreakingBranching =
    new PermutationBreakingBranching(variables, varHeuristic, valHeuristic)
}

/**
  * branching which dynamically breaks the symmetry of permutations of values. It avoids exploring symmetrical
  * solutions while preserving the variable ordering given by the variable heuristic
  * @param variables the list of variables to branch on
  * @param varHeuristic given the index of a variable, give a score for it. the lowest scores will be selected first
  * @param valHeuristic scores the values. those with lowest score will be selected first.
  */
class PermutationBreakingBranching(variables: Array[CPIntVar],
                                   varHeuristic: Int => Int,
                                   valHeuristic: (Int, Int) => Int) extends Branching {
  private val context = variables(0).context

  private def selectMin[T](list: Array[T])(condition: T => Boolean)(order: T => Int): Option[T] = {
    var i = 0
    var bestValue = Int.MaxValue
    var bestI = -1
    while (i < list.length) {
      val v = order(list(i))
      if (condition(list(i)) && v < bestValue) {
        bestI = i
        bestValue = v
      }
      i += 1
    }

    if (bestI == -1) None
    else Some(list(bestI))
  }

  override def alternatives(): Seq[_root_.oscar.algo.search.Alternative] = {
    selectMin(variables.indices)(!variables(_).isBound)(varHeuristic) match {
      case Some(i) => {
        val x = variables(i)
        val maxUsed = variables.maxBoundOrElse(-1)
        (x.getMin to maxUsed+1).filter(x.hasValue).sortBy(valHeuristic(i, _)).map(Decision.assign(x, _))
      }
      case None => noAlternative
    }
  }
}
