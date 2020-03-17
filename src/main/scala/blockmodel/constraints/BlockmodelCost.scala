package blockmodel.constraints

import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.algo.search
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

  private val n = X.length // the number of vertices in the graph
  private val k = M.length // the number of positions in the model

  // this is for the Delta objects associated with the different constraints on the violation between two blocks
  private val costsDeltas = Array.ofDim[DeltaIntVar](k, k)

  private val unboundVertices = new ReversibleSparseSet(s, 0, n-1)

  private val nb1Bound = Array.fill(k,k)(new ReversibleInt(s, 0))
  private val nb0Bound = Array.fill(k,k)(new ReversibleInt(s, 0))
  private val nb1UnboundCol = Array.fill(k,n)(new ReversibleInt(s, 0))
  private val nb0UnboundCol = Array.fill(k,n)(new ReversibleInt(s, 0))
  private val nb1UnboundRow = Array.fill(n,k)(new ReversibleInt(s, 0))
  private val nb0UnboundRow = Array.fill(n,k)(new ReversibleInt(s, 0))

  private val delta = Array.fill(n,k)(new ReversibleInt(s, 0))

  def delta(i: Int, r: Int): Int = delta(i)(r).value


  override def associatedVars(): Iterable[CPVar] = M.toVector.flatten ++ C ++ cost.toVector.flatten :+ totalCost

  override def setup(l: CPPropagStrength): Unit = {

    for (r <- 0 until k; s <- 0 until k) {
      costsDeltas(r)(s) = cost(r)(s).callPropagateOnChangesWithDelta(this)
      M(r)(s).callPropagateWhenBind(this)
    }

    for (i <- 0 until n) C(i).callPropagateWhenBind(this)

    totalCost.callPropagateWhenBoundsChange(this)

    propagate()

  }

  def updateCounters(): Unit = {
    var x = unboundVertices.size-1
    while (x >= 0) {
      val i = unboundVertices(x)
      if (C(i).isBound) {
        unboundVertices.removeValue(i)
        val r = C(i).value
        // update the unbound counters
        var y = unboundVertices.size-1
        while (y >= 0){
          val j = unboundVertices(y)
          // a link i->j becomes a link r->j
          if (X(i)(j))  nb1UnboundCol(r)(j) += 1
          else          nb0UnboundCol(r)(j) += 1
          // a link j->i becomes a link j->r
          if (X(j)(i))  nb1UnboundRow(j)(r) += 1
          else          nb0UnboundRow(j)(r) += 1

          y -= 1
        }
        // update the bound counters
        var s = 0
        while (s < k) {
          // for the row r, we add the rows
          nb0Bound(r)(s) += nb0UnboundRow(i)(s)
          nb1Bound(r)(s) += nb1UnboundRow(i)(s)
          // for the column r, we add the columns
          nb0Bound(s)(r) += nb0UnboundCol(s)(i)
          nb1Bound(s)(r) += nb1UnboundCol(s)(i)
          s += 1
        }

        if (X(i)(i))  nb1Bound(r)(r) += 1
        else          nb0Bound(r)(r) += 1

      }
      x -= 1
    }
  }

  def filterCost(): Unit = {
    var minTotalCost = 0
    var r = 0
    while (r < k) {
      var s = 0
      while (s < k) {
        val minCost: Int =
          if (! M(r)(s).isBound) min(nb0Bound(r)(s), nb1Bound(r)(s))
          else if (M(r)(s).value == 0) nb1Bound(r)(s)
          else /* M(r)(s).value == 1*/ nb0Bound(r)(s)

        minTotalCost += minCost
        cost(r)(s).updateMin(minCost)
        s += 1
      }
      r += 1
    }

    var x = 0
    while (x < unboundVertices.size) {
      val i = unboundVertices(x)
      var r = 0
      while (r < k) {
        if (C(i).forall(M(r)(_) isBoundTo 0)) // if this row can only go to a 0 block, we add the number of 1
          minTotalCost += nb1UnboundRow(i)(r)
        else if (C(i).forall(M(r)(_) isBoundTo 1)) // and if it can only go to a 1 block, the number of 0
          minTotalCost += nb0UnboundRow(i)(r)
        else  minTotalCost += min(nb0UnboundRow(i)(r), nb1UnboundRow(i)(r)) // otherwise the min is a safe bet
        // same thing for the columns
        if (C(i).forall(M(_)(r).isBoundTo(0)))
          minTotalCost += nb1UnboundCol(r)(i)
        else if (C(i).forall(M(_)(r).isBoundTo(1)))
          minTotalCost += nb0UnboundCol(r)(i)
        else  minTotalCost += min(nb0UnboundCol(r)(i), nb1UnboundCol(r)(i))
        r += 1
      }
      x += 1
    }
    totalCost.updateMin(minTotalCost)
    if (unboundVertices.isEmpty && M.flatten.forall(_.isBound))
      totalCost.updateMax(minTotalCost)
  }

  def filterB(): Unit = {
    var r = 0
    while (r < k) {
      var s = 0
      while (s < k) {
        if (!M(r)(s).isBound) {
          if (nb0Bound(r)(s) > cost(r)(s).getMax) M(r)(s).removeValue(1)
          if (nb1Bound(r)(s) > cost(r)(s).getMax) M(r)(s).removeValue(0)
        }
        s += 1
      }
      r += 1
    }
  }

  def filterC(): Unit = {
    // for every unbound vertex i, we will calculate the updated costs of adding it to position r
    //for (i <- unboundVertices; r <- C(i); s <- 0 until b) {
    var x = 0
    while (x < unboundVertices.size) {
      val i = unboundVertices(x)
      val domain = C(i).toArray
      var y = 0
      while (y < domain.length) {
        val r = domain(y)

        var Δir = 0

        var s = 0
        while (s < k) {
          if (costsDeltas(r)(s).maxChanged) {
            if (r == s) {
              val newNb0 = nb0Bound(r)(s).value + nb0UnboundRow(i)(s) + nb0UnboundCol(s)(i) + (if (X(i)(i)) 0 else 1)
              val newNb1 = nb1Bound(r)(s).value + nb1UnboundRow(i)(s) + nb1UnboundCol(s)(i) + (if (X(i)(i)) 1 else 0)

              if (M(r)(r).isBoundTo(1)) Δir += newNb0 - nb0Bound(r)(s)
              else if (M(r)(r).isBoundTo(0)) Δir += newNb1 - nb1Bound(r)(s)
              else Δir += min(newNb0 - nb0Bound(r)(s), newNb1 - nb1Bound(r)(s))

              if ((M(r)(r).isBoundTo(1) && newNb0 > cost(r)(r).getMax) ||
                (M(r)(r).isBoundTo(0) && newNb1 > cost(r)(r).getMax) ||
                min(newNb1, newNb0) > cost(r)(r).getMax) {
                C(i).removeValue(r)
              }
            } else {
              // for the row r
              val newNb0_rs = nb0Bound(r)(s).value + nb0UnboundRow(i)(s)
              val newNb1_rs = nb1Bound(r)(s).value + nb1UnboundRow(i)(s)

              if (M(r)(s).isBoundTo(1)) Δir += nb0UnboundRow(i)(s)
              else if (M(r)(s).isBoundTo(0)) Δir += nb1UnboundRow(i)(s)
              else Δir += min(nb0UnboundRow(i)(s), nb1UnboundRow(i)(s))
              // for the column r
              val newNb0_sr = nb0Bound(s)(r).value + nb0UnboundCol(s)(i)
              val newNb1_sr = nb1Bound(s)(r).value + nb1UnboundCol(s)(i)

              if (M(s)(r).isBoundTo(1)) Δir += nb0UnboundCol(s)(i)
              else if (M(r)(s).isBoundTo(0)) Δir += nb1UnboundCol(s)(i)
              else Δir += min(nb0UnboundCol(s)(i), nb1UnboundCol(s)(i))

              if ((M(r)(s).isBoundTo(1) && newNb0_rs > cost(r)(s).getMax) ||
                (M(r)(s).isBoundTo(0) && newNb1_rs > cost(r)(s).getMax) ||
                min(newNb0_rs, newNb1_rs) > cost(r)(s).getMax ||
                (M(s)(r).isBoundTo(1) && newNb0_sr > cost(s)(r).getMax) ||
                (M(s)(r).isBoundTo(0) && newNb1_sr > cost(s)(r).getMax) ||
                min(newNb0_sr, newNb1_sr) > cost(s)(r).getMax) {
                C(i).removeValue(r)
              }
            }
          }
          s += 1
        }

        delta(i)(r) := Δir
        y += 1
      }
      x += 1
    }
  }

  def biggestCostDelta(vertex: Int): Int = C(vertex).maxBy(delta(vertex)(_).value)

  def smallestCostDelta(vertex: Int): Int = C(vertex).minBy(delta(vertex)(_).value)

  def worstFirstValueOrder(vertex: Int): Array[Int] =
    C(vertex).toArray.sortBy(-delta(vertex)(_).value)

  def bestFirstValueOrder(vertex: Int): Array[Int] =
    C(vertex).toArray.sortBy(delta(vertex)(_).value)

  override def propagate(): Unit = {
    updateCounters()
    filterCost()
    filterB()
    filterC()
  }

  def heuristic(): Seq[search.Alternative] = {
    // take the unbound vertex with minimal domain size if it exists, None otherwise
    C.filterNot(_.isBound).reduceOption((a, b) => if (a.size <= b.size) a else b) match {
      case Some(v) => // there is an unbound vertex, so we bind it first
        val maxUsed = C.maxBoundOrElse(-1)
        branchAll((0 to maxUsed + 1).filter(v.hasValue))(value => s.add(v === value))
      case None => // all vertices have been bound, we bind the M values
        (for(x <- 0 until k; y <- 0 until k) yield (x,y)).find({case (x,y) => !M(x)(y).isBound}) match {
          case Some((x,y)) =>
            if (nb0Bound(x)(y) < nb1Bound(x)(y)) branch(s.add(M(x)(y) === 1))(s.add(M(x)(y) !== 1))
            else branch(s.add(M(x)(y) === 0))(s.add(M(x)(y) !== 0))
          case None => noAlternative
        }
    }
  }
}
