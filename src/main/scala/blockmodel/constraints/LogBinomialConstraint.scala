package blockmodel.constraints

import java.math.{MathContext, RoundingMode}

import blockmodel.utils.BigDecimalDoubleVar
import oscar.cp._

import math.log

object LogBinomialConstraint {
  /**
    * returns a new variable that is constrained to be
    * log( C(n, k) ) where C(n,k) is the binomial coefficient for n and k.
    * The returned variable is a Double Var with a fixed number of decimal positions,
    * implemented in BigDecimalDoubleVar. This number of decimals is given by the optional
    * parameter precision.
    */
  def logBinomial(n: Int, k: CPIntVar, precision: Int = 6): BigDecimalDoubleVar = {
    def logC(n: Int, k: Int): BigDecimal = {
      if (k < 0 || k > n)   throw new IllegalArgumentException()
      if (k == 0 || k == n) return 0
      val kmin = math.min(k, n - k)
      var c = BigDecimal(0)
      for (i <- 0 until kmin) {
        c = c + BigDecimal(log(n - i)) - BigDecimal(log(i+1))
      }
      c
    }

    val tuplesBigDecimal = k.map(i => (i, logC(n, i)))
    val exp = BigDecimal(10).pow(precision)

    println(s"Binomial n=$n: ", tuplesBigDecimal.mkString(", "))
    val tuplesInt = tuplesBigDecimal map {
      case (key, v) => (key, (v * exp).setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP).intValue)
    }
    println(s"Binomial n=$n: ", tuplesInt.mkString(", "))
    val x = new BigDecimalDoubleVar(
      vMin = tuplesBigDecimal.map(_._2).min,
      vMax = tuplesBigDecimal.map(_._2).max,
      precision)(k.store)

    println(s"fresh x variable is $x, underlying is ${x.v}")

    k.store.add(table(k, x.v, tuplesInt))
    x
  }
}