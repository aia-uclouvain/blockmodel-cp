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

  var verbose: Boolean = false

  private val n = X.length // the number of vertices in the graph
  private val k = M.length // the number of positions in the model

  // this is for the Delta objects associated with the different constraints on the violation between two blocks
  private val costsDeltas = Array.ofDim[DeltaIntVar](k, k)

  private val unboundVertices = new ReversibleSparseSet(s, 0, n-1)

  // nb1Bound(c,d) is the number of edges between vertices bound to cluster c and vertices bound to cluster d
  // i.e. the number of 1s in the submatrix whose lines are vertices bound to c and columns bound to d
  private val nb1Bound = Array.fill(k,k)(new ReversibleInt(s, 0))
  // the number of 0 in the same submatrix. It will always be #{(v,w) | C(v) = c && C(w) = d} - nb1Bound(c,d).
  private val nb0Bound = Array.fill(k,k)(new ReversibleInt(s, 0))
  // nb1UnboundCol(c,v) is the number of edges between vertices bound to cluster c and vertex v, i.e. the number of 1
  // in the subcolumn whose lines are vertices bound to c and whose column index is v. We only keep the value up to date
  // for unbound vertices.
  private val nb1UnboundCol = Array.fill(k,n)(new ReversibleInt(s, 0))
  private val nb0UnboundCol = Array.fill(k,n)(new ReversibleInt(s, 0))
  // nb1UnboundRow is the number of edges between vertex v and vertices bound to cluster c, i.e. the number of 1 in the
  // subrow whose row index is v and columns are vertices bound to c. We only keep the value up to date for unbound
  // vertices.
  private val nb1UnboundRow = Array.fill(n,k)(new ReversibleInt(s, 0))
  private val nb0UnboundRow = Array.fill(n,k)(new ReversibleInt(s, 0))

  // delta(v,c) will store the lower bound on the cost added to the solution when assigning vertex v to cluster c
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

  // update the different counters of 1s and 0s after vertices are bound.
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
        if (verbose && minCost > cost(r)(s).getMin) println(s"cost$r$s lb is $minCost")
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
        if (C(i).forall(M(_)(r) isBoundTo 0)) { // if this row can only go to a 0 block, we add the number of 1
          if (verbose) println(s"the row from C$i to cluster $r will always fall in a null block, so we add the number of 1s = ${nb1UnboundRow(i)(r)}")
          minTotalCost += nb1UnboundRow(i)(r)
        }
        else if (C(i).forall(M(_)(r) isBoundTo 1)) { // and if it can only go to a 1 block, the number of 0
          if (verbose) println(s"the row from C$i to cluster $r will always fall in a full block, so we add the number of 0s = ${nb0UnboundRow(i)(r)}")
          minTotalCost += nb0UnboundRow(i)(r)
        }
        else  { // otherwise the min is a safe bet
          if (verbose && min(nb0UnboundRow(i)(r), nb1UnboundRow(i)(r)) > 0) println(s"vertex $i row $r will always cost at least ${min(nb0UnboundRow(i)(r), nb1UnboundRow(i)(r))}")
          minTotalCost += min(nb0UnboundRow(i)(r), nb1UnboundRow(i)(r))
        }
        // same thing for the columns
        if (C(i).forall(M(r)(_).isBoundTo(0))) {
          if (verbose) println(s"the col from cluster $r to C$i will always fall in a null block, so we add the number of 1s = ${nb1UnboundCol(r)(i)}")
          minTotalCost += nb1UnboundCol(r)(i)
        }
        else if (C(i).forall(M(r)(_).isBoundTo(1))) {
          if (verbose) println(s"the col from cluster $r to C$i will always fall in a full block, so we add the number of 0s = ${nb0UnboundCol(r)(i)}")
          minTotalCost += nb0UnboundCol(r)(i)
        }
        else  {
          if (verbose && min(nb0UnboundCol(r)(i), nb1UnboundCol(r)(i)) > 0) println(s"vertex $i col $r will always cost at least ${min(nb0UnboundCol(r)(i), nb1UnboundCol(r)(i))}")
          minTotalCost += min(nb0UnboundCol(r)(i), nb1UnboundCol(r)(i))
        }
        r += 1
      }
      x += 1
    }
    if (verbose && minTotalCost > totalCost.getMin) println(s"totalcost lb is $minTotalCost")
    totalCost.updateMin(minTotalCost)
    if (unboundVertices.isEmpty && M.flatten.forall(_.isBound))
      totalCost.updateMax(minTotalCost)
  }

  def filterM(): Unit = {
    var r = 0
    while (r < k) {
      var s = 0
      while (s < k) {
        if (!M(r)(s).isBound) {
          if (nb0Bound(r)(s) > cost(r)(s).getMax) {
            if (verbose) println(s"M$r$s cannot be 1")
            M(r)(s).removeValue(1)
          }
          if (nb1Bound(r)(s) > cost(r)(s).getMax) {
            if (verbose) println(s"M$r$s cannot be 0")
            M(r)(s).removeValue(0)
          }
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
                if (verbose) println(s"C$i cannot be in cluster $r")
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
                if (verbose) println(s"C$i cannot be in cluster $r")
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

  def biggestCostDelta(vertex: Int): Int = {
    var best = C(vertex).getMin
    var max = delta(vertex)(best)
    var i = best+1
    while (i < C(vertex).getMax) {
      if (C(vertex).hasValue(i) && delta(vertex)(i) > max) {
        max = delta(vertex)(i)
        best = i
      }
      i += 1
    }
    best
  }

  def smallestCostDelta(vertex: Int): Int = C(vertex).minBy(delta(vertex)(_).value)

  def maxCostDelta(vertex: Int): Int = C(vertex).map(delta(vertex)(_).value).max
  def minCostDelta(vertex: Int): Int = C(vertex).map(delta(vertex)(_).value).min
  def sumCostDelta(vertex: Int): Int = C(vertex).map(delta(vertex)(_).value).sum

  def worstFirstValueOrder(vertex: Int): Array[Int] =
    C(vertex).toArray.sortBy(-delta(vertex)(_).value)

  def bestFirstValueOrder(vertex: Int): Array[Int] =
    C(vertex).toArray.sortBy(delta(vertex)(_).value)

  // variable heuristic for the M variables, to be used with oscar's Idx algorithms. Given the index of a M variable
  // in M.flatten, it will give the lowest cost achievable for that variable
  def MVarHeuris(i: Int): Int = {
    val x = i/k
    val y = i%k
    min(nb0Bound(x)(y),nb1Bound(x)(y))
  }

  // value heuristic for the M variables, to be used with oscar's Idx algorithms. Given the index of a M variable
  // in M.flatten, it will give the value (0 or 1) that will minimize the cost if bound to it.
  def MValHeuris(i: Int): Int = {
    val x = i/k
    val y = i%k
    if (nb0Bound(x)(y) > nb1Bound(x)(y)) 0
    else 1
  }

  override def propagate(): Unit = {
    updateCounters()
    if(verbose) printState()
    filterCost()
    filterM()
    filterC()
  }

  def printState(): Unit = {
    import blockmodel.Blockmodel
    import blockmodel.utils.Digraph
    import blockmodel.utils.Matrix._
    val tmpC = C.map(v => if (v.isBound) v.value else k)
    val tmpM = Array.fill(k+1,k+1)(false)
    val bm = new Blockmodel(tmpC, tmpM)
    val g = new Digraph(X)
    println("here is the state of the assignment (last group is unassigned)")
    println(bm.toStringGrouped(g))
    println("with the image matrix")
    println(M.toStringMatrix)

    println("number of 0")
    println(nb0Bound.toStringMatrix)

    println("number of 1")
    println(nb1Bound.toStringMatrix)

    val counts = Array.tabulate(k+unboundVertices.size,k+unboundVertices.size)((i,j) => {
      val uv = (0 until n).filterNot(C(_).isBound)
      if(i < k && j < k) nb1Bound(i)(j) + "/" + nb0Bound(i)(j)
      else if(i < k && j >= k) nb1UnboundCol(i)(uv(j-k)) + "/" + nb0UnboundCol(i)(uv(j-k))
      else if(i >= k && j < k) nb1UnboundRow(uv(i-k))(j) + "/" + nb0UnboundRow(uv(i-k))(j)
      else "···"
    })

    println(counts.toStringMatrix.replace('0', '·'))
  }
}
