package blockmodel.utils

import scala.math.log

object BinomaialCoefficient {
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

  /** binary search for the last index of `arr` where a condition `cond` holds
    * mapping cond on the array should be monotonic, i.e.
    * arr.map(cond) = [true true ... true false false ... false]
    * if the conditions holds for no value, -1 is returned
    *  e.g. search(Array(false, false), identity(_)) = -1
    * if the condition holds for all values, the last index is returned
    *  e.g. search((0 until 100).toArray, _ > 0) = 100
    *
    * this is useful to search through the binomial coefficient tables
    */
  def search[T](arr: Array[T], cond: T => Boolean): Int = {
    def s(start: Int = 0, end: Int = arr.length -1): Int = {
      val mid = start + (end-start)/2
      if (!cond(arr(start)))     start - 1
      else if (cond(arr(end)))   end
      else if (cond(arr(mid)))   s(mid+1, end)
      else                       s(start, mid-1) // narrow the field
    }
    s()
  }

}
