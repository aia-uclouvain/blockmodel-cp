import blockmodel.Blockmodel
import blockmodel.constraints.LogBinomialConstraint._
import blockmodel.constraints.OnesInSubmatrix
import blockmodel.utils.Digraph
import org.scalatest._
import oscar.cp._

import scala.math._

class OnesInSubmatrixTest extends FunSuite {


  test("constructed example col==row") {
    implicit val s = CPSolver()

    val n = 7
    val k = 2
    val X = Array(
      //    0 1 2 3 4 5 6
      Array(1,0,0,1,0,0,0),
      Array(0,0,0,1,0,1,0),
      Array(1,1,1,1,0,1,1),
      Array(0,0,0,1,0,0,0),
      Array(0,1,0,0,1,0,0),
      Array(0,0,0,1,0,0,1),
      Array(0,0,0,1,0,0,0)
    ).map(_.map(_ == 1))

    val g = new Digraph(X)

    val C = Array.fill(n)(CPIntVar(0, k))
    C(2).assign(0)
    C(4).assign(0)

    val n1 = CPIntVar(0, 16)
    val c = new OnesInSubmatrix(n1, g, C, 0, 0)
    add(c)
    def printC(): Unit =
      println((0 until n).map(i=>s"$i: ${C(i)}").mkString("\n"))
    println("***updating n1 to [11,16]")
    n1.updateMin(11)
    s.update()
    println("***removing line 5")
    C(5).removeValue(0)
    s.update()
    assert(C(0) hasValue 0)
    assert(C(1) hasValue 0)
    assert(C(3) hasValue 0)
    assert(C(6) hasValue 0)
    assert(n1 hasValue 12)
  }

  test("constructed example col!=row") {
    implicit val s = CPSolver()

    val n = 7
    val k = 2
    val X = Array(
      //    0 1 2 3 4 5 6
      Array(1,0,0,1,0,0,0),
      Array(0,0,0,1,0,1,0),
      Array(1,1,1,1,0,1,1),
      Array(0,0,0,1,0,0,0),
      Array(0,1,0,0,1,0,0),
      Array(0,0,0,1,0,0,1),
      Array(0,0,0,1,0,0,0)
    ).map(_.map(_ == 1))

    val g = new Digraph(X)

    val C = Array.fill(n)(CPIntVar(0, k))
    C(2).assign(0)
    C(4).assign(1)

    val n1 = CPIntVar(0, 16)
    val c = new OnesInSubmatrix(n1, g, C, 0, 1)
    add(c)
    c.debug=true
    def printC(): Unit =
      println((0 until n).map(i=>s"$i: ${C(i)}").mkString("\n"))
    println("***start state")
    println(s"n = $n1")
    printC()
    println("***updating n1 to [8,16]")
    n1.updateMin(8)
    s.update()
    println(s"n = $n1")
    printC()

  }


  test("finding the true max") {
    implicit val s = CPSolver()

    val n = 7
    val k = 2
    val X = Array(
      //    0 1 2 3 4 5 6
      Array(1,0,0,1,0,0,0),
      Array(0,0,0,1,0,1,0),
      Array(1,1,1,1,0,1,1),
      Array(0,0,0,1,0,0,0),
      Array(0,1,0,0,1,0,0),
      Array(0,0,0,1,0,0,1),
      Array(0,0,0,1,0,0,0)
    ).map(_.map(_ == 1))

    val g = new Digraph(X)

    val C = Array.fill(n)(CPIntVar(0, k))
    C(2).assign(0)
    C(4).assign(1)
    C(3).removeValue(1)
    val n1 = CPIntVar(0, 16)
    val c = new OnesInSubmatrix(n1, g, C, 0, 1)
    def printC(): Unit =
      println((0 until n).map(i=>s"$i: ${C(i)}").mkString("\n"))
    add(c)
    s.maximize(n1)
    s.onSolution(printC())
    s.search(binary(C))
    s.start
    println(s.statistics)

  }

}
