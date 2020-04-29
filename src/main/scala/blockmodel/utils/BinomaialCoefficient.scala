package blockmodel.utils

import scala.math.log

object BinomaialCoefficient  extends App {
  def logC(n: Int, k: Int): Double = {
    if (k < 0 || k > n)   throw new IllegalArgumentException()
    if (k == 0 || k == n) return 0
    val kmin = math.min(k, n - k)
    var c = 0.0
    for (i <- 0 until kmin) {
      c = c + log(n - i) - log(i + 1)
    }
    c
  }

  def CTable(n: Int): Array[Double] = {
    val res: Array[Double] = Array.ofDim(n / 2 + 1)

    var coef = 1.0
    for (k <- res.indices) {
      res(k) = coef
      coef = coef * (n - k) / (k + 1)
    }
    res
  }

  def logCTable(n: Int): Array[Double] = {
    val res: Array[Double] = Array.ofDim(n / 2 + 1)

    var coef = 0.0
    for (k <- res.indices) {
      res(k) = coef
      coef += math.log((n - k).toDouble / (k + 1)) / math.log(2)
    }
    res
  }
}
