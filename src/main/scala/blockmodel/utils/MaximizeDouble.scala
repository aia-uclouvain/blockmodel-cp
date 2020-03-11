package blockmodel.utils

import oscar.cp.CPStore
import oscar.cp.constraints.CPObjective
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar

/**
  * maximise a double var in oscar
  */
class MaximizeDouble(cp: CPStore, obj: DoubleVar, epsilon: Double = 1e-6) extends
  CPObjective(cp) {
  override def associatedVars(): Iterable[CPVar] = Array[CPVar]()

  var best = obj.min - epsilon

  override def setup(l: CPPropagStrength): Unit = {}

  override def propagate(): Unit = obj.updateMin(best + epsilon)

  def updateLB(newLB: Double): Unit = best = best.max(newLB)

  override def tighten(): Unit = {
    if (!obj.isBound(epsilon))
      throw new RuntimeException("objective not bound:" + obj)
    updateLB(obj.min + epsilon)
  }

  override def isOptimum(): Boolean = false

  def restart(): Unit = {
    best = obj.min - epsilon
  }
}
