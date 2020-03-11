package blockmodel.constraints

import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.cp._
import oscar.cp.core._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPVar

import scala.math.{max, min}

/**
  * Global constraint for one-mode blockmodeling for structural equivalence. Given a directed graph described
  * by the adjacency matrix X, this constraint filters the values of the block model described by the image
  * matrix M and the cluster assignment C, while ensuring that
  *
  * (i) the reconstruction error sum_{i,j} | X(i)(j) - M(C(i))(C(j)) | <= totalCost
  * (i) for any two clusters c, d, the block cost sum_{i,j} (C(i) = c)·(C(j) = d)·| X(i)(j) - M(c)(d) | <= cost(c)(d)
  *
  * @param X the adjacency matrix of the graph to be modelled.
  *          X(i)(j) == true if there is an arc from i to j, false otherwise
  * @param M CP variable representing the image matrix.
  *          M(c)(d) = 1 if there is an arc between cluster c and cluster d, 0 otherwise
  * @param C CP variable representing the assignment of vertices into clusters.
  *          C(i) = c if vertex i is assigned to cluster c
  * @param cost for any pair of cluster c,d : cost(c)(d) is a CP variable representing the cost of this block
  *             cost(c)(d) = x if there are x wrongly predicted arcs from vertices in cluster c to vertices in d.
  * @param totalCost CP variable representing the total reconstruction cost of the graph given the blockmodel described
  *                  by M and C.
  */
class BlockmodelCost(X: Array[Array[Boolean]], M: Array[Array[CPBoolVar]], C: Array[CPIntVar],
                     cost: Array[Array[CPIntVar]], totalCost: CPIntVar)
  extends Constraint(C(0).store, "BlockmodelCost") {

  protected val n: Int = X.length // the number of vertices in the graph
  protected val k: Int = M.length // the number of positions in the model

  // this is for the Delta objects associated with the different blockmodel.constraints on the violation between two blocks
  protected val CDeltas: Array[DeltaIntVar] = Array.ofDim[DeltaIntVar](n)

  protected val unboundVertices = new ReversibleSparseSet(s, 0, n-1)

  protected val nb1Block: Array[Array[ReversibleInt]] = Array.fill(k,k)(new ReversibleInt(s, 0))
  protected val nb0Block: Array[Array[ReversibleInt]] = Array.fill(k,k)(new ReversibleInt(s, 0))

  protected val lbTotalCost = new ReversibleInt(s, 0)

  protected val nb1Col: Array[Array[ReversibleInt]] = Array.fill(k,n)(new ReversibleInt(s, 0))
  protected val nb0Col: Array[Array[ReversibleInt]] = Array.fill(k,n)(new ReversibleInt(s, 0))

  protected val nb1Row: Array[Array[ReversibleInt]] = Array.fill(n,k)(new ReversibleInt(s, 0))
  protected val nb0Row: Array[Array[ReversibleInt]] = Array.fill(n,k)(new ReversibleInt(s, 0))

  protected val minSize: Array[ReversibleInt] = Array.fill(k)(new ReversibleInt(s, 0))
  protected val maxSize: Array[ReversibleInt] = Array.fill(k)(new ReversibleInt(s, n))

  // a lower bound on the cost of block c,d given current variables nb{1,0}*
  @inline def lbCost(c: Int, d: Int): Int = {
    if (M(c)(d) isBoundTo 1) nb0Block(c)(d)
    else if (M(c)(d) isBoundTo 0) nb1Block(c)(d)
    else min(nb0Block(c)(d), nb1Block(c)(d))
  }

  // a lower bound on the added cost to block c,d of assinging vertex i to cluster x
  // it is accurate only for vertices i in unboundVertices
  def lbAssignment(i: Int, x: Int, c: Int, d: Int): Int = {
    if (x == c) {
      if (x == d) { // x == c == d
        if (M(c)(d) isBoundTo 1)
          nb0Col(c)(i).value + nb0Row(i)(c).value + (if (X(i)(i)) 0 else 1)
        else if (M(c)(d) isBoundTo 0)
          nb1Col(c)(i).value + nb1Row(i)(c).value + (if (X(i)(i)) 1 else 0)
        else min(
          nb0Col(c)(i).value + nb0Row(i)(c).value + (if (X(i)(i)) 0 else 1),
          nb1Col(c)(i).value + nb1Row(i)(c).value + (if (X(i)(i)) 1 else 0)
        )
      } else { // x == c, x != d
        if (M(c)(d) isBoundTo 1) nb0Row(i)(d)
        else if (M(c)(d) isBoundTo 0) nb1Row(i)(d)
        else min(nb0Row(i)(d), nb1Row(i)(d))
      }
    } else {
      if (x == d) { // x != c, x == d
        if (M(c)(d) isBoundTo 1) nb0Col(c)(i)
        else if (M(c)(d) isBoundTo 0) nb1Col(c)(i)
        else min(nb0Col(c)(i), nb1Col(c)(i))
      } else { // x != c, x != d
        0
      }
    }
  }

  // an upper bound on the added cost to block c,d of assinging vertex i to cluster x
  // it is accurate only for vertices i in unboundVertices
  def ubAssignment(i: Int, x: Int, c: Int, d: Int): Int = {
    if (x == c) {
      if (x == d) { // x == c == d
        if (M(c)(d) isBoundTo 1)
          nb0Col(c)(i).value + nb0Row(i)(c).value + (if (X(i)(i)) 0 else 1)
        else if (M(c)(d) isBoundTo 0)
          nb1Col(c)(i).value + nb1Row(i)(c).value + (if (X(i)(i)) 1 else 0)
        else max(
          nb0Col(c)(i).value + nb0Row(i)(c).value + (if (X(i)(i)) 0 else 1),
          nb1Col(c)(i).value + nb1Row(i)(c).value + (if (X(i)(i)) 1 else 0)
        )
      } else { // x == c, x != d
        if (M(c)(d) isBoundTo 1) nb0Row(i)(d)
        else if (M(c)(d) isBoundTo 0) nb1Row(i)(d)
        else max(nb0Row(i)(d), nb1Row(i)(d))
      }
    } else {
      if (x == d) { // x != c, x == d
        if (M(c)(d) isBoundTo 1) nb0Col(c)(i)
        else if (M(c)(d) isBoundTo 0) nb1Col(c)(i)
        else max(nb0Col(c)(i), nb1Col(c)(i))
      } else { // x != c, x != d
        0
      }
    }
  }

  protected val delta = Array.fill(n,k)(new ReversibleInt(s, 0))

  override def associatedVars(): Iterable[CPVar] = M.toVector.flatten ++ C ++ cost.toVector.flatten :+ totalCost

  override def setup(l: CPPropagStrength): Unit = {
    for (r <- 0 until k; s <- 0 until k) {
      cost(r)(s).callPropagateWhenBoundsChange(this)
      M(r)(s).callPropagateWhenBind(this)
    }
    for (i <- 0 until n) CDeltas(i) = C(i).callPropagateOnChangesWithDelta(this)
    totalCost.callPropagateWhenBoundsChange(this)

    propagate()
  }

  @inline def minDeltaC(vertex: Int): Int =
    if (vertex >= 0 && vertex < n) C(vertex).minBy(delta(vertex)(_).value)
    else 0

  @inline def minDeltaM(c: Int, d: Int): Int =
    if (nb0Block(c)(d) < nb1Block(c)(d)) 1
    else 0

  private val clustersChanged = Array.ofDim[Boolean](k)

  // a lower bound on the cost incurred by arcs going from vertex i to vertices in cluster c
  // it is accurate only for vertices i in unboundVertices
  @inline def lbRowCost(c: Int, i: Int): Int =
    if (nb0Row(i)(c) < nb1Row(i)(c)) nb0Row(i)(c)
    else nb1Row(i)(c)

  // a lower bound on the cost incurred by arcs going from vertices in cluster c to vertex i
  // it is accurate only for vertices i in unboundVertices
  @inline def lbColCost(c: Int, i: Int): Int =
    if (nb0Col(c)(i) < nb1Col(c)(i)) nb0Col(c)(i)
    else nb1Col(c)(i)

  def updateCounters(): Unit = {
    var x = unboundVertices.size-1
    while (x >= 0) {
      val i = unboundVertices(x)

      if (C(i).isBound) {
        unboundVertices.removeValue(i)

        val r = C(i).value
        clustersChanged(r) = true

        // update the unbound counters
        var y = unboundVertices.size-1
        while (y >= 0){
          val j = unboundVertices(y)
          // a link i->j becomes a link r->j
          if (X(i)(j))  nb1Col(r)(j) += 1
          else          nb0Col(r)(j) += 1
          // a link j->i becomes a link j->r
          if (X(j)(i))  nb1Row(j)(r) += 1
          else          nb0Row(j)(r) += 1

          y -= 1
        }
        // update the bound counters
        var s = 0
        while (s < k) {
          // for the row r, we add the rows
          nb0Block(r)(s) += nb0Row(i)(s)
          nb1Block(r)(s) += nb1Row(i)(s)
          // for the column r, we add the columns
          nb0Block(s)(r) += nb0Col(s)(i)
          nb1Block(s)(r) += nb1Col(s)(i)
          s += 1
        }

        if (X(i)(i))  nb1Block(r)(r) += 1
        else          nb0Block(r)(r) += 1

      }
      x -= 1
    }
  }


  private val clusters = Array.ofDim[Int](k)

  def updateMinMaxSize(): Unit = {
    var i = 0
    while (i < n) {
      val delta = CDeltas(i)
      if (delta.changed) {
        if (C(i).isBound) minSize(C(i).value) += 1
        delta.fillArray(clusters)
        var j = 0
        while (j < delta.size) {
          val c = clusters(j)
          maxSize(c) -= 1
          j += 1
        }
      }
      i += 1
    }
  }

  def filterM(): Unit = {
    var r = 0
    while (r < k) {
      if (clustersChanged(r)) {
        var s = 0
        while (s < k) {
          if (!M(r)(s).isBound) {
            if (nb0Block(r)(s) > cost(r)(s).getMax) {
              M(r)(s).assignFalse()
            }
            if (nb1Block(r)(s) > cost(r)(s).getMax) {
              M(r)(s).assignTrue()
            }
          }
          if (!M(s)(r).isBound) {
            if (nb0Block(s)(r) > cost(s)(r).getMax) {
              M(s)(r).assignFalse()
            }
            if (nb1Block(s)(r) > cost(s)(r).getMax) {
              M(s)(r).assignTrue()
            }
          }
          s += 1
        }
      }

      r += 1
    }
  }

  def filterC(): Unit = {
    var x = 0
    while (x < unboundVertices.size) {
      val i = unboundVertices(x)
      C(i).fillArray(clusters)
      var y = 0
      while (y < C(i).getSize) {
        val c = clusters(y)
        if (clustersChanged(c)) {
          // this will store the sum for all blocks of the added cost of assigning vertex i to cluster c
          var Δic = 0
          // we check if the added costs exceeds cost(c)(d) for any d
          var d = 0
          while (d < k && C(i).hasValue(c)) {
            val δic_cd = lbAssignment(i, c, c, d) // added cost for block c,d of assigning i to c
            Δic += δic_cd
            if (lbCost(c, d) + δic_cd > cost(c)(d).getMax) {
              C(i).removeValue(c)
            }
            // do the same for δic_dc, but only if c != d
            if (c != d) {
              val δic_dc = lbAssignment(i, c, d, c) // added cost for block d,c of assigning i to c
              Δic += δic_dc
              if (lbCost(d, c) + δic_dc > cost(d)(c).getMax) {
                C(i).removeValue(c)
              }
            }
            d += 1
          }
          if (lbTotalCost.value + Δic > totalCost.getMax) {
            C(i).removeValue(c)
          }

          delta(i)(c) := Δic
        }

        y += 1
      }
      x += 1
    }
  }

  def filterCost(): Unit = {
    var minTotalCost = 0
    var c = 0
    while (c < k) {
      var d = 0
      while (d < k) {
        val minCostCD = lbCost(c,d)
        minTotalCost += minCostCD

        cost(c)(d).updateMin(minCostCD)

        val x1 = minSize(c).value
        val x2 = maxSize(c).value - minSize(c)
        val y1 = minSize(d).value
        val y2 = maxSize(d).value - minSize(d)
        val unknownSize = x1*y2 + y1*x2 + x2*y2

        if (M(c)(d).isBound) {
          cost(c)(d).updateMax(minCostCD + unknownSize)
        }
        else {
          cost(c)(d).updateMax(max(nb1Block(c)(d), nb0Block(c)(d)) + unknownSize)
        }

        d += 1
      }
      c += 1
    }

    lbTotalCost := minTotalCost

    var x = 0
    while (x < unboundVertices.size) {
      val i = unboundVertices(x)
      var c = 0
      while (c < k) {
        minTotalCost += lbColCost(c,i) + lbRowCost(c,i)
        c += 1
      }
      x += 1
    }
    totalCost.updateMin(minTotalCost)
  }

  override def propagate(): Unit = {

    // reset all flags of clustersChanged to false.
    java.util.Arrays.fill(clustersChanged, false)

    updateCounters()
    filterCost()
    filterM()
    filterC()
    updateMinMaxSize()
  }
}
