package blockmodel.utils

import java.math.MathContext

import oscar.cp.CPIntVar
import oscar.cp.core.{CPStore, Constraint}

import java.math.RoundingMode

/**
  * double var backed up with a CPIntvar with a fixed number of decimal places
  * @param vMin min value for the domain of the variable
  * @param vMax max value for the domain of the variable
  * @param precision number of decimals to store
  */
class BigDecimalDoubleVar(vMin: BigDecimal, vMax: BigDecimal, precision: Int)(override implicit val store: CPStore)
  extends DoubleVar(epsilon = math.pow(10, -precision)) {

  private val mc = new MathContext(precision, RoundingMode.HALF_UP)
  private val exp = BigDecimal(10).pow(precision)
  assert(precision >= 0, "precision can't be negative")
  val v = CPIntVar(
    (vMin * exp).setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP).intValue,
    (vMax * exp).setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP).intValue
  )

  def double2int(d: Double): Int = (BigDecimal(d) * exp).setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP).intValue
  def int2double(i: Int): Double = (BigDecimal(BigInt(i)) / exp).toDouble

  override def updateMin(newMin: Double): Unit = v.updateMin(double2int(newMin))
  override def updateMax(newMax: Double): Unit = v.updateMax(double2int(newMax))
  override def isBound(epsilon: Double): Boolean = v.isBound

  override def min: Double = int2double(v.getMin)
  override def max: Double = int2double(v.getMax)
  override def value: Double = int2double(v.value)

  override def isBound: Boolean = v.isBound

  override def callPropagateWhenBoundsChange(c: Constraint): Unit = v.callPropagateWhenBoundsChange(c)

  override def toString: String =
    if (isBound) value.toString
    else s"[$min, $max]"
}
