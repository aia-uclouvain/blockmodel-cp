package blockmodel.utils

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleDouble, ReversibleInt}
import oscar.cp.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPVar
import oscar.cp.core.watcher.WatcherListL2

/**
  * double var implemented as a range variable with two reversible doubles
  * @param baseMin min value of the domain
  * @param baseMax max value of the domain
  * @param epsilon when max-min < epsilon, the variable is considered to be bound
  */
class DoubleVarImpl(baseMin: Double, baseMax: Double, epsilon: Double = 1e-6, name: String = "")
                   (implicit override val store: CPStore) extends DoubleVar(epsilon, name) {
  private[this] val vmin = new ReversibleDouble(store, baseMin)
  private[this] val vmax = new ReversibleDouble(store, baseMax)

  // Registered constraints
  //private[this] val onBindL2 = new WatcherListL2(cp)
  private[this] val onBoundsL2 = new WatcherListL2(store)
  //private[this] val onDomainL2 = new WatcherListL2(cp)

    //private[this] val onBindL1 = new WatcherListL1(cp)
  //private[this] val onBoundsL1 = new WatcherListL1(cp)
  //private[this] val onDomainL1 = new WatcherListL1(cp)

    // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often

  if (vmin > vmax) throw Inconsistency

  @inline
  override def almostEqual(a: Double, b: Double): Boolean = Math.abs(a - b) < epsilon

  @inline
  def updateMin(newMin: Double): Unit = {
    if (vmax.value < newMin && !almostEqual(vmax.value, newMin))
      throw new Inconsistency {
        override def feedback: Any = "New value " + newMin + " but max is " +
          vmax.value
      }
    if (vmin.value < newMin) {
      vmin.value = newMin
      onBoundsL2.enqueue()
    }
  }

  @inline
  def updateMax(newMax: Double): Unit = {
    if (vmin.value > newMax && !almostEqual(vmin.value, newMax)) {
      throw new Inconsistency {
        override def feedback: Any = "New value " + newMax + " but min is " +
          vmin.value
      }
    }
    if (vmax.value > newMax) {
      vmax.value = newMax
      onBoundsL2.enqueue()
    }
  }

  def assign(value: Double): Unit = {
    if (vmin.value > value || vmax.value < value)
      throw Inconsistency
    if (!almostEqual(vmin.value, vmax.value)) {
      onBoundsL2.enqueue()
      vmin.value = value
      vmax.value = value
    }
  }
  def min = vmin.value

  def max = vmax.value

  def value = {
    if (!almostEqual(vmin.value, vmax.value))
      throw new Exception("unbound %s %s".format(min, max))
    vmin.value
  }
  override def isBound = Math.abs(vmin.value - vmax.value) <= epsilon

  def callPropagateWhenBoundsChange(c: Constraint) {
    degree.incr()
    onBoundsL2.register(c)
  }

  override def isBound(epsilon: Double): Boolean = Math.abs(vmin.value - vmax.value) <= epsilon
}