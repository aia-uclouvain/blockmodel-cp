import collection.mutable.Stack
import org.scalatest._
import oscar.cp._
import blockmodel.constraints.LogBinomialConstraint._
import math._

class logBinomialConstraintTest extends FunSuite {

  val precision = 1e-6

  test("logBinomial for n=0") {
    implicit val s = CPSolver()

    val n = 0
    val k = CPIntVar(0)
    val x = logBinomial(n,k)
    assert(abs(x.min) < precision)
    assert(abs(x.max) < precision)

    assert(x.isBound)
  }

  test("logBinomial for n=1") {
    implicit val s = CPSolver()

    val n = 1
    val k = CPIntVar(0 to n)
    val x = logBinomial(n,k)
    assert(abs(x.min) < precision)
    assert(abs(x.max) < precision)

    assert(x.isBound)
  }

  test("logBinomial for n=2") {
    implicit val s = CPSolver()

    val n = 2
    val k = CPIntVar(0 to n)
    val x = logBinomial(n,k)

    val min = x.min
    val max = x.max
    assert(abs(min) < precision)
    assert(abs(max - log(2)) < precision, s" max is $max, should be log(2) = ${log(2)}")

    assert(!x.isBound)

    s.add(k !== 1)
    assert(x.isBound)
    assert(abs(x.value) < precision)
  }

  test("logBinomial for n=8") {implicit val s = CPSolver()
    val n = 8
    val k = CPIntVar(0 to n)
    val x = logBinomial(n,k)
    // binomial coefficients for n = 8
    //k=  0     1     2     3     4     5     6     7     8
    //x= 	1  		8  		28 		56 		70 		56 		28 		8  		1

    assert(abs(x.min - log(1)) < precision)
    assert(abs(x.max - log(70)) < precision, s" max is ${x.max}, should be log(70) = ${log(70)}")
    // 424850
    // 4248495
    // 0.42485
    s.add(k !== 0)
    //  	x  		8  		28 		56 		70 		56 		28 		8  		1
    assert(abs(x.min - log(1)) < precision)
    assert(abs(x.max - log(70)) < precision)

    s.add(k !== n)
    //  	x  		8  		28 		56 		70 		56 		28 		8  		x
    assert(abs(x.min - log(8)) < precision)

    x.updateMax(log(27))
    s.update()
    //k=  0     1     2     3     4     5     6     7     8
    //x= 	x  		8  		xx 		xx 		xx 		xx 		xx 		8  		x
    assert(k.toSet == Set(1, 7))
  }

  test("logBinomial for n=100") {
    implicit val s = CPSolver()
    val n = 100
    val k = CPIntVar(0 to 10)
    val x = logBinomial(n, k)

    assert(abs(x.min) < precision)
    assert(abs(x.max - log(17310309456440.0)) < precision)

    s.add(k <= 9)
    assert(abs(x.max - log(1902231808400.0)) < precision)

    x.updateMax(log(10000000000.0))
    s.update()

    assert(k.getMax <= 6)

  }
}
