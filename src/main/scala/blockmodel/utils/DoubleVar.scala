package blockmodel.utils

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleDouble, ReversibleInt}
import oscar.cp.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.watcher.WatcherListL2

abstract class DoubleVar(val epsilon: Double = 1e-6, val name: String = "")
               (implicit val store: CPStore) extends CPVar {

  def updateMin(newMin: Double): Unit
  def updateMax(newMax: Double): Unit
  def isBound(epsilon: Double): Boolean
  def min: Double
  def max: Double
  def value: Double

  def callPropagateWhenBoundsChange(c: Constraint): Unit

  @inline
  def almostEqual(a: Double, b: Double): Boolean = Math.abs(a - b) < epsilon

  override def toString: String = if (isBound) s"$value" else s"[$min, $max]"
//
//
//  def *(x: Double): DoubleVar = new DoubleVarImpl(epsilon*x) {
//    override def updateMin(newMin: Double): Unit = super.updateMin(newMin/x)
//    override def updateMax(newMax: Double): Unit = super.updateMax(newMax/x)
//    override def isBound(epsilon: Double): Boolean = super.isBound(epsilon/x)
//    override def min: Double = x * super.min
//    override def max: Double = x * super.max
//    override def value: Double = x * super.value
//    override def callPropagateWhenBoundsChange(c: Constraint): Unit = super.callPropagateWhenBoundsChange(c)
//    override def isBound: Boolean = super.isBound
//  }
//
//  def unary_-(): DoubleVar = new DoubleVarImpl() {
//    override def updateMin(newMin: Double): Unit = super.updateMax(-newMin)
//    override def updateMax(newMax: Double): Unit = super.updateMin(-newMax)
//    override def isBound(epsilon: Double): Boolean = super.isBound(epsilon)
//    override def min: Double = -super.max
//    override def max: Double = -super.min
//    override def value: Double = -super.value
//    override def callPropagateWhenBoundsChange(c: Constraint): Unit = super.callPropagateWhenBoundsChange(c)
//    override def isBound: Boolean = super.isBound
//  }
}

object DoubleVar {

  def apply(min: Double, max: Double, epsilon: Double = 1e-6)(implicit c: CPStore): DoubleVar = {
    if (max-min > epsilon) new DoubleVarImpl(min, max, epsilon)
    else DoubleVar(min)
  }

  def apply(fixedValue: Double)(implicit c: CPStore): DoubleVar = new DoubleVar() {
    override def updateMin(newMin: Double): Unit =
      if (value < newMin) throw new Inconsistency {
        override def feedback: Any = "New value " + newMin + " but max is " + value
      }
    override def updateMax(newMax: Double): Unit =
      if (value > newMax) throw new Inconsistency {
        override def feedback: Any = "New value " + newMax + " but min is " + value
      }
    override def isBound(epsilon: Double): Boolean = true
    override val min: Double = fixedValue
    override val max: Double = fixedValue
    override val value: Double = fixedValue
    override def callPropagateWhenBoundsChange(c: Constraint): Unit = {}
    override val isBound: Boolean = true
  }

  import math.{toIntExact,ceil,floor}
  implicit def CPIntVarToDoubleVar(v: CPIntVar)(implicit s: CPStore): DoubleVar = new DoubleVar() {
    override def updateMin(newMin: Double): Unit = v.updateMin(toIntExact(ceil(newMin).toLong))
    override def updateMax(newMax: Double): Unit = v.updateMax(toIntExact(floor(newMax).toLong))
    override def isBound(epsilon: Double): Boolean = v.isBound
    override def min: Double = v.getMin
    override def max: Double = v.getMax
    override def value: Double = v.value
    override def callPropagateWhenBoundsChange(c: Constraint): Unit = v.callPropagateWhenBoundsChange(c)
    override def isBound: Boolean = v.isBound
  }

  def log(v: DoubleVar)(implicit s: CPStore): DoubleVar = new DoubleVar() {
    override def updateMin(newMin: Double): Unit = v.updateMin(math.exp(newMin))
    override def updateMax(newMax: Double): Unit = v.updateMax(math.exp(newMax))
    override def isBound(epsilon: Double): Boolean = v.isBound(epsilon)
    override def min: Double = math.log(v.min)
    override def max: Double = math.log(v.max)
    override def value: Double = math.log(v.value)
    override def callPropagateWhenBoundsChange(c: Constraint): Unit = v.callPropagateWhenBoundsChange(c)
    override def isBound: Boolean = v.isBound
  }
}