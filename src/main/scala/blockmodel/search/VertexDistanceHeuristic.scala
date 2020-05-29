package blockmodel.search

import blockmodel.utils.Digraph
import oscar.algo.reversible.{ReversibleDouble, ReversibleInt}
import oscar.cp._
import oscar.cp.core.watcher.Watcher

class VertexDistanceHeuristic(C: Array[CPIntVar], k: Int, g: Digraph) {
  implicit val s = C(0).store

  private val vertices = (0 until g.n)
  val clusterSize = Array.fill(k)(new ReversibleInt(s, 0))
  val clusterRowSum = Array.fill(k,g.n)(new ReversibleInt(s, 0))
  val clusterColSum = Array.fill(k,g.n)(new ReversibleInt(s, 0))
  val distances = Array.fill(g.n,k)(new ReversibleDouble(s, 0))

  for (i <- C.indices if C(i).isBound) {
    val cluster = C(i).value
    clusterSize(cluster) += 1
    for (j <- vertices if g(i,j)) clusterRowSum(cluster)(j) += 1
    for (j <- vertices if g(j,i)) clusterColSum(cluster)(j) += 1
  }
  for (i <- C.indices if !C(i).isBound; c <- C(i)) {
    distances(i)(c) := distToCluster(i, c)
  }
  for (i <- C.indices if !C(i).isBound) {
    C(i).awakeOnChanges(() => {
      if (C(i).isBound) {
        val cluster = C(i).value
        clusterSize(cluster) += 1
        for (j <- vertices if g(i, j)) clusterRowSum(cluster)(j) += 1
        for (j <- vertices if g(j, i)) clusterColSum(cluster)(j) += 1

        for (j <- C.indices if !C(j).isBound && C(j).hasValue(cluster))
          distances(j)(cluster) := distToCluster(j, cluster)
      }
    })
  }



  // distance is the number of different entries in the row x and row y + number of different entries in the col x + col y
  def dist(x: Int, y: Int): Int = vertices.count(i => g(x,i) != g(y,i)) + vertices.count(i => g(i,x) != g(i,y))


  def distToCluster(x: Int, c: Int): Double = {
    if (clusterSize(c).value == 0) 0
    else {
      var i = 0
      var sum = 0.0
      while (i < vertices.length) {
        val dxi = math.abs((if(g(x, i)) 1 else 0) - clusterRowSum(c)(i).toDouble / clusterSize(c))
        val dix = math.abs((if(g(i, x)) 1 else 0) - clusterColSum(c)(i).toDouble / clusterSize(c))
        sum += dxi + dix
        i += 1
      }
      sum
    }
  }

  def vertexMinDist(v: Int): Double = {
    C(v).map(distances(v)(_).value).min
  }
  def vertexMaxDist(v: Int): Double = {
    C(v).map(distances(v)(_).value).max
  }

  def bestCluster(v: Int): Int = {
    C(v).minBy(distances(v)(_).value)
  }


}