package blockmodel.constraints

import blockmodel.utils.Digraph
import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.cp.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPVar}

class OnesInSubmatrix(n: CPIntVar, X: Digraph, C: Array[CPIntVar], row: Int, col: Int)
extends Constraint(C(0).store){

  val CDeltas: Array[DeltaIntVar] = Array.ofDim(C.length)
  val possibleVars = new ReversibleSparseSet(s, 0, X.n-1)
  val rowMin: Array[ReversibleInt] = Array.fill(X.n)(new ReversibleInt(s, 0))
  val rowMax: Array[ReversibleInt] = Array.fill(X.n)(new ReversibleInt(s, X.e))
  val colMin: Array[ReversibleInt] = Array.fill(X.n)(new ReversibleInt(s, 0))
  val colMax: Array[ReversibleInt] = Array.fill(X.n)(new ReversibleInt(s, X.e))
  val nmin = new ReversibleInt(s, 0)
  val nmax = new ReversibleInt(s, X.e)

  var debug = false


  override def associatedVars(): Iterable[CPVar] = ???

  def count(): Unit = {
    for (i <- 0 until X.n if C(i) hasValue row) {
      rowMax(i) := (0 until X.n).count(j => (row==col || i!=j) && X(i, j) && C(j).hasValue(col))
      rowMin(i) := (0 until X.n).count(j => (row==col || i!=j) && X(i, j) && C(j).isBoundTo(col))
    }
    if(debug) {
      println("rows: min max")
      println((0 until X.n).map(i => f"$i%4d: ${rowMin(i).value}%3d ${rowMax(i).value}%3d " +
        (if(C(i).isBoundTo(row)) "✓" else if(!C(i).hasValue(row)) "❌" else "?")).mkString("\n"))
      println(f"   Σ: ${(0 until X.n).filter(C(_).hasValue(row)).map(rowMin(_).value).sum}%3d " +
        f"${(0 until X.n).filter(C(_).hasValue(row)).map(rowMax(_).value).sum}%3d")
    }
    for (j <- 0 until X.n if C(j) hasValue col) {
      colMax(j) := (0 until X.n).count(i => (row==col || i!=j) && X(i, j) && C(i).hasValue(row))
      colMin(j) := (0 until X.n).count(i => (row==col || i!=j) && X(i, j) && C(i).isBoundTo(row))
    }
    if(debug) {
      println("cols: min max")
      println((0 until X.n).map(i => f"$i%4d: ${colMin(i).value}%3d ${colMax(i).value}%3d "+
        (if(C(i).isBoundTo(col)) "✓" else if(!C(i).hasValue(col)) "❌" else "?")).mkString("\n"))
      println(f"   Σ: ${(0 until X.n).filter(C(_).hasValue(col)).map(colMin(_).value).sum}%3d " +
        f"${(0 until X.n).filter(C(_).hasValue(col)).map(colMax(_).value).sum}%3d")
    }
    var min = 0
    var max = 0
    for(i <- 0 until X.n; j <- 0 until X.n if (row==col || i!=j) && X(i,j)) {
      if (C(i).hasValue(row) && C(j).hasValue(col)) max += 1
      if (C(i).isBoundTo(row) && C(j).isBoundTo(col)) min += 1
    }
    if(debug) println(s"global min: $min, global max: $max")
    n.updateMin(min)
    n.updateMax(max)
    nmin := min
    nmax := max
  }

  override def setup(l: CPPropagStrength): Unit = {
    // setup
    count()


    // register
    n.callPropagateWhenBoundsChange(this)
    for (i <- C.indices) {
      if (!C(i).isBound && (C(i).hasValue(row) || C(i).hasValue(col))) {
        CDeltas(i) = C(i).callPropagateOnChangesWithDelta(this)
      } else {
        possibleVars.removeValue(i)
      }
    }
  }

  override def propagate(): Unit = {
    // todo work in progress version
    var idx = possibleVars.size
    while(idx > 0) {
      idx -= 1
      val x = possibleVars(idx)
      val variable = C(x)
      val delta = CDeltas(x)
      // check which values have been removed (row, col or both)
      for(v <- delta.values) {
        if (v == row)
          for (i <- 0 until X.n if X(x, i)) {
            colMax(i) -= 1
            nmax -= 1
          }
        // todo check row==col case
        if (v == col) for (i <- possibleVars if X(i, x)) {
          rowMax(i) -= 1
          nmax -= 1
        }
      }
      if (variable.isBoundTo(row)) {
        for (i <- possibleVars if X(x, i)) colMin(i) += 1
        for (i <- 0 until X.n if C(i).isBoundTo(col) && X(x,i)) nmin -= 1
      } // todo check row==col case
      if (variable.isBoundTo(col)) {
        for (i <- possibleVars if X(i, x)) rowMin(i) += 1
        for (i <- 0 until X.n if C(i).isBoundTo(row) && X(i,x)) nmin -= 1
      }

      if (variable.isBound || (!variable.hasValue(row) && !variable.hasValue(col)))
        possibleVars.removeValue(idx)
    }

    // todo update nmin and nmax

    // todo replace this by an incremental count based on the deltas
    //count()
    // todo we can do more pruning if there is only one vertex left unbound
    if (row != col) {
      for (i <- 0 until X.n if !C(i).isBound && (C(i) hasValue row)) {
        if (nmax.value - rowMax(i) < n.getMin) {
          C(i).assign(row); if (debug) println(s"$i is assigned to row $row")
        }
        if (nmin.value + rowMin(i) > n.getMax) {
          C(i).removeValue(row); if (debug) println(s"$i cannot be assigned to row $row")
        }
      }
      for (i <- 0 until X.n if !C(i).isBound && (C(i) hasValue col)) {
        if (nmax.value - colMax(i) < n.getMin) {
          C(i).assign(col); if (debug) println(s"$i is assigned to col $col")
        }
        if (nmin.value + colMin(i) > n.getMax) {
          C(i).removeValue(col); if (debug) println(s"$i cannot be assgined to col $col")
        }
      }
    }
    else { // here, row=col so we either take the row and col corresponding to the vertex, or discard them both
      // in this way, we have to look at the sum between row and col values, with sometimes an extra +1
      // if there is a self loop on i, i.e. X(i,i) = true
      for (i <- 0 until X.n if !C(i).isBound && (C(i) hasValue row)) {
        val p = if(X(i,i)) 1 else 0
        if (nmax.value - rowMax(i) - colMax(i) + p < n.getMin) {
          C(i).assign(row); if (debug) println(s"$i is assigned to row/col $row")
        }
        if (nmin.value + rowMin(i) + colMin(i) + p > n.getMax) {
          C(i).removeValue(row); if (debug) println(s"$i cannot be assigned to row $row")
        }
      }
    }
  }


}
