package blockmodel.search

import blockmodel.utils.Digraph
import oscar.cp._
import oscar.cp.core.watcher.Watcher

class VertexDistanceHeuristic(C: Array[CPIntVar], k: Int, g: Digraph) {
  private val vertices = (0 until g.n)
  val clusterSize = Array.fill(k)(0)
  val clusterRowSum = Array.fill(k)(Array.fill(g.n)(0))
  val clusterColSum = Array.fill(k)(Array.fill(g.n)(0))

  for (i <- C.indices if C(i).isBound) {
    val cluster = C(i).value
    clusterSize(cluster) += 1
    for (j <- vertices if g(i,j)) clusterRowSum(cluster)(j) += 1
    for (j <- vertices if g(j,i)) clusterColSum(cluster)(j) += 1
  }
  for (i <- C.indices if !C(i).isBound) {
    C(i).awakeOnChanges(() => {
      if (C(i).isBound) {
        val cluster = C(i).value
        clusterSize(cluster) += 1
        for (j <- vertices if g(i, j)) clusterRowSum(cluster)(j) += 1
        for (j <- vertices if g(j, i)) clusterColSum(cluster)(j) += 1
      }
    })
  }



  // distance is the number of different entries in the row x and row y + number of different entries in the col x + col y
  def dist(x: Int, y: Int): Int = vertices.count(i => g(x,i) != g(y,i)) + vertices.count(i => g(i,x) != g(i,y))
  def distToCluster(x: Int, c: Int): Int = {
    if (clusterSize(c) == 0) 0
    else {
      vertices.map(i => math.abs((if(g(x, i)) 1 else 0) - clusterRowSum(c)(i) / clusterSize(c))).sum
      + vertices.map(i => math.abs((if(g(i, x)) 1 else 0) - clusterColSum(c)(i) / clusterSize(c))).sum
    }
  }

  def vertexMinDist(v: Int): Int = {
    C(v).map(distToCluster(v, _)).min
  }
  def vertexMaxDist(v: Int): Int = {
    C(v).map(distToCluster(v, _)).max
  }

  def bestCluster(v: Int): Int = {
    C(v).minBy(distToCluster(v, _))
  }


}