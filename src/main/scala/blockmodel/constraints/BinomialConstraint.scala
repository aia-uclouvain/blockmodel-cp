package blockmodel.constraints

import oscar.cp._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPVar
import blockmodel.utils.DoubleVar

object BinomialConstraint {
  // calculate the binomial coeficieft n chose k, as per the wikipedia page
  def binomial_coefficient(n: Int, k: Int): Int = {
    if (k < 0 || k > n)   return 0
    if (k == 0 || k == n) return 1
    val kmin = math.min(k, n - k)
    var c = 1
    for (i <- 0 until kmin) {
      c = c * (n - i) / (i + 1)
    }
    c
  }
  /**
    * returns a fresh variable x with the constraints that
    * x = C(n,k)
    */
  def apply(n: Int, k: CPIntVar): CPIntVar = {
    // create the list of tuples (i, n chose i) for every value in the domain of k
    val tuples = k.map(i => (i, binomial_coefficient(n, i)))
    // create the variable for x
    val x = CPIntVar(tuples.map(_._2))(k.store)
    // constrain x = n C k
    x.store.add(table(k, x, tuples))
    x // return the fresh variable
  }
}