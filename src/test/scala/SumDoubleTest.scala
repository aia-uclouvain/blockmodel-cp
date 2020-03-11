import blockmodel.constraints.SumDouble
import org.scalatest.FunSuite
import oscar.cp.core.CPSolver
import blockmodel.utils.DoubleVar
import math.abs
class SumDoubleTest extends FunSuite {

  test("sum1") {
    val cp = CPSolver()
    val x = Array(DoubleVar(0, 0.5)(cp), DoubleVar(0.1, 0.5)(cp), DoubleVar(0, 0.5)(cp))
    val y = DoubleVar(0, 100)(cp)
    cp.add(new SumDouble(x, y))
    assert(abs(y.min-0.1) < y.epsilon)
    assert(abs(y.max-1.5) < y.epsilon)
  }

  test("sum2") {
    val cp = CPSolver()
    val x = Array(DoubleVar(-0.5, 0.5)(cp), DoubleVar(0.1, 0.2)(cp), DoubleVar(0.0, 0.1)(cp))
    val y = DoubleVar(0,100)(cp)
    cp.add(new SumDouble(x, y))
    assert(abs(x(0).min - -0.3) < x(0).epsilon)
    assert(abs(y.min) < y.epsilon)
    assert(abs(y.max - 0.8) < y.epsilon)
  }

  test("sum3") {
    val cp = CPSolver()
    val x = Array(DoubleVar(0, 0.5)(cp), DoubleVar(0, 0.5)(cp), DoubleVar(0, 0.5)(cp))
    val y = DoubleVar(0.5)(cp)
    cp.add(new SumDouble(x, y))
    x(1).updateMin(0.0)
    x(1).updateMax(0.0)
    x(0).updateMin(0.1)
    cp.update()

    //    [0.1,0.5] + 0.0 + [0.0, 0.5] = 0.5
    // => [0.1,0.5] + 0.0 + [0.0, 0.4] = 0.5

    //x(0).max should be(0.5)
    assert(abs(x(0).max - 0.5) < x(0).epsilon)
    //x(2).min should be(0.0)
    assert(abs(x(2).min - 0.0) < x(2).epsilon)
    //x(2).max should be(0.4)
    assert(abs(x(2).max - 0.4) < x(2).epsilon)
  }

//  val rand = new scala.util.Random(0)
//  def solve(x: Array[CPIntVar], y: CPIntVar, decomp: Boolean = false): Int = {
//    val cp = y.store.asInstanceOf[CPSolver]
//    //cp.pushState()
//    var nbSol = 0
//    if (decomp) cp.add(new oscar.cp.constraints.Sum(x, y))
//    else cp.add(sum(x, y))
//    cp.search {
//      binaryStatic(x)
//    } onSolution {
//      nbSol += 1
//    }
//    cp.start
//    cp.popAll()
//    nbSol
//
//  }
//
//  test("sum4") {
//    val cp = CPSolver()
//    val x = Array(CPIntVar(-2 to 5, "x0")(cp), CPIntVar(-4 to 5, "x1")(cp), CPIntVar(3 to 5, "x2")(cp))
//    val y = CPIntVar(4 to 5, "y")(cp)
//    solve(x, y, false) should be(solve(x, y, true))
//  }
//
//  test("sum5") {
//    val cp = CPSolver()
//    val x = Array(CPIntVar(Set(-5, -3, 2, 8))(cp), CPIntVar(Set(-10, 8))(cp), CPIntVar(3 to 5)(cp))
//    val y = CPIntVar(3 to 8)(cp)
//    solve(x, y, false) should be(solve(x, y, true))
//  }
//
//  test("sum6") {
//    val cp = CPSolver()
//    val x = Array(CPIntVar(Set(-5, -3, 2, 8))(cp), CPIntVar(Set(-10, 8))(cp), CPIntVar(3 to 5)(cp), CPIntVar(-10 to -5)(cp))
//    val y = CPIntVar(3 to 8)(cp)
//    solve(x, y, false) should be(solve(x, y, true))
//  }
}
