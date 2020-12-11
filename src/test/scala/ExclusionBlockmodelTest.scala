import blockmodel.{Blockmodel, ExclusionBlockmodelCPModel, ExclusionBlockmodelCPModel2}
import blockmodel.utils.Digraph
import org.scalatest.FunSuite
import oscar.cp._

import scala.util.Random

class ExclusionBlockmodelTest extends FunSuite {
  test("randomized") {
    val k = 3
    val n = 20
    val rng = new Random()
    val error = 0.05

    val maxNbExcluded = 9

    for (i <- 0 to 10) {
      val (g, bm) = Digraph.randomWithImage(Blockmodel.ringStructure(k), n, rng, error)
      val img = bm.toImageGrouped(g, 10)

      var sols = Seq[Blockmodel]()
      // model with k+1 clusters. the last cluster, with index k, is the excluded variables.
      object model1 extends ExclusionBlockmodelCPModel(g, k) {
        add(nbExcluded <= maxNbExcluded)
        search(binaryFirstFail(C) ++ binary(M.flatten.asInstanceOf[Array[CPIntVar]]))
        onSolution {
          val bm = getBlockmodel
          //println(bm.toStringGrouped(g))
          sols :+= bm
        }
      }

      println("*** solving with old model")
      val s1 = model1.solver.start()
      println(s1)

      var sols2 = Seq[Blockmodel]()
      object model2 extends ExclusionBlockmodelCPModel2(g, k) {
        add(nbExcluded <= maxNbExcluded)
        search(binaryFirstFail(C) ++ binary(M.flatten.asInstanceOf[Array[CPIntVar]]))
        onSolution {
          val bm = getBlockmodel
          //println(bm.toStringGrouped(g))
          sols2 :+= bm
        }
      }

      println("*** solving with new model")
      val s2 = model2.solver.start()
      println(s2)

      val trustedSolutions = sols.toSet
      val otherSolutions = sols2.toSet

      val falseSolutions = otherSolutions &~ trustedSolutions
      val unfoundSolutions = trustedSolutions &~ otherSolutions

      assert(falseSolutions.isEmpty, s"there were ${falseSolutions.size} false solutions.")
      assert(unfoundSolutions.isEmpty)

      assert(trustedSolutions == otherSolutions)

      println(s"time gain is ${s1.time - s2.time}ms, ${((s1.time - s2.time).toDouble / s1.time * 100).toInt}%\n\n")
    }
  }
}
