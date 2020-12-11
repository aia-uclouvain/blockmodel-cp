package blockmodel

import java.awt.{Color, Image}
import java.awt.image.BufferedImage

import utils.Digraph
import utils.Matrix._

import scala.util.Random

/**
  * class representing a block model, storing its clusters and its image matrix.
  * @param C the cluster index for every vertex in the graph. vertex v is in cluster i <=> C(v)=i
  * @param M image matrix for the block model
  */
class Blockmodel(val C: Array[Int], val M: Array[Array[Boolean]]){
  val n: Int = C.length // number of vertices
  val k: Int = M.length // number of clusters
  for (row <- M if row.length != k) throw new IllegalArgumentException("M is not a square matrix")
  for (c <- C if c < 0 || c >= k) throw new IllegalArgumentException(s"$c cannot be a vertex label if k = $k")

  lazy val modelDescriptionLength: Double = {
    import scala.math.log
    log(n) / log(2) + n * log(k) / log(2) + k * k + log(n * n) / log(2)
  }

  // utility function for summation
  private def Σij(ito: Int, jto: Int)(f: (Int, Int) => Int): Int = {
    var i = 0
    var sum = 0
    while (i < ito) {
      var j = 0
      while (j < jto) {
        sum += f(i,j)
        j += 1
      }
      i += 1
    }
    sum
  }

  def cost(g: Digraph): Int = {
    var i = 0
    var sum = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        if (g(i)(j) != M( C(i) )( C(j) )) sum += 1
        j += 1
      }
      i += 1
    }
    sum
  }

  def costOfVertices(g: Digraph): Array[Int] = {
    val res = Array.fill(n)(0)
    for(i<-0 until n;j<-0 until n if g(i)(j) != M(C(i))(C(j))) {
      res(i) += 1
      res(j) += 1
    }
    res
  }
  def costOfVertex(v: Int, g: Digraph): Int = {
    var cost = 0
    var i = 0
    while (i < n) {
      if(g(v)(i) != M( C(v) )( C(i) )) cost += 1
      if(g(i)(v) != M( C(i) )( C(v) )) cost += 1
      i += 1
    }
    cost
  }
  def costOfCluster(c: Int, g: Digraph): Int = {
    var cost = 0
    for (i <- 0 until n; j <- 0 until n if C(i)==c || C(j) == c) {
      cost += (if (g(i)(j) == M(C(i))(C(j))) 0 else 1)
    }
    cost
  }

  def worstCluster(g: Digraph): Int = (0 until k).maxBy(costOfCluster(_, g))

  override def hashCode(): Int = toString.hashCode
  def sameElements(a: Array[Int], b: Array[Int]): Boolean = ! a.indices.exists(i => a(i) != b(i))
  override def equals(obj: Any): Boolean = obj match {
    case that: Blockmodel =>
      if (this.n != that.n || this.k != that.k) false
      else
        sameElements(this.C, that.C) && (this.M sameMatrix that.M)
    case _ => false
  }

  override def toString: String = C.mkString(",") + "\n" + M.mapCells(if(_) "1" else "0").toStringMatrix + "\n"
  /**
    * Get a string representing the matrix where the columns and rows have been reordered according to
    * the assignment in groups passed as argument.
    */
  def toStringGrouped(g: Digraph): String = {
    // groupedVertices is a sequence of sequences of vertex ids. The first sequence will be the ids of the
    // first group, and so on.
    val groupedVertices = (0 to C.max).map(r => C.indices.filter(C(_) == r))
    val cellSize = n.toString.length
    val maxNameSize = (0 until n).map(g.names).map(_.length).max
    val fmt = s"%${cellSize}s"
    val nameFmt = s"%${maxNameSize + cellSize + 1}s"
    val result = new StringBuilder()
    // print the header for the columns
    result append nameFmt.format("") + "┃" + groupedVertices.map(group =>
      group.map(id => fmt.format(id)).mkString(" "))
      .mkString("┃")
    result append "\n"
    for (group <- groupedVertices) {
      result append ("━" * (maxNameSize + cellSize + 1)) + "╋" +
        groupedVertices.map(group => "━" * ((cellSize + 1) * group.length - 1)).mkString("╋")
      result append "\n"
      for (i <- group) {
        result append nameFmt.format(g.names(i)+" "+i) + "┃"
        result append groupedVertices.map(group =>
          group.map(j =>
            if (g(i)(j)) fmt.format("1") else fmt.format("·")
          ).mkString(" ")
        ).mkString("┃")
        result append "\n"
      }
    }
    result.toString()
  }

  // returns an image equivalent to toStringGrouped, with clusters separated by red lines and ties shown as black pixels
  def toImageGrouped(g: Digraph, scale:Int = 1): BufferedImage = {
    val s = (g.n + k)
    val size = scale * s
    val img = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val white = Color.WHITE.getRGB
    val red   = Color.RED.getRGB
    val black = Color.BLACK.getRGB

    val groupedVertices = (0 until k).map(r => C.indices.filter(C(_) == r))
    var pixels = List[Int]()
    for (group <- groupedVertices) {
      // horizontal separator
      pixels ++= List.fill(s)(red)
      for (i <- group) {
        // fill the line for vertex i
        for (group2 <- groupedVertices) {
          pixels :+= red
          for (j <- group2) {
            if (g(i)(j)) pixels :+= black
            else pixels :+= white
          }
        }
      }
    }
    for(x <- 0 until size; y <- 0 until size)
      img.setRGB(x, y, pixels( (x/scale) + (y/scale)*s) )
    img
  }
}

object Blockmodel {
  /**
    * generates an image graph with a ring structure, e.g.
    *    *→*
    *   ↗   ↘
    *  *     *
    *   ↖   ↙
    *    *←*
    * i.e. every cluster i is connected to the cluster i+1, and the last cluster is connected to the first one
    * @param size the number of clusters
    * @param directed wether the image graph must be directed or undirected = symmetrical
    * @return the adjacency matrix for the image graph
    */
  def ringStructure(size: Int, directed: Boolean = true): Array[Array[Boolean]] = {
    val res = Array.fill(size, size)(false)
    for (i <- 0 until size) {
      res(i)((i+1) % size) = true
      if (!directed) res((i+1) % size)(i) = true
    }
    res
  }

  /**
    * generates an image graph with a stick structure, e.g.
    * *->*->*->*->*
    * @param size the number of clusters in the image matrix
    * @param directed whether the image graph must be directed or undirected = symmetrical
    * @return the adjacency matrix for the image graph
    */
  def stickStructure(size: Int, directed: Boolean = true): Array[Array[Boolean]] = {
    val res = Array.fill(size, size)(false)
    for (i <- 0 until size-1) {
      res(i)(i+1) = true
      if (!directed) res(i+1)(i) = true
    }
    res
  }
  /**
    * generates an image graph with a star structure, e.g.
    *      *↺
    *      ↑
    *  ↻*← *↺→*↺
    *      ↓
    *      *↺
    * i.e. distinct communities, all related to a central community
    *
    * @param size the number of clusters in the image matrix
    * @param directed whether the image graph must be directed or undirected = symmetrical
    * @return the adjacency matrix for the image graph
    */
  def starStructure(size: Int, directed: Boolean = true): Array[Array[Boolean]] = {
    val res = Array.fill(size, size)(false)
    for (i <- 0 until size) {
      res(i)(i) = true
      res(0)(i) = true
      if (!directed) res(i)(0) = true
    }
    res
  }

  /**
    * generates an image with a community structure (i.e. each cluster is a clique)
    * @param size number of clusters in the image graph
    * @return the adjacency matrix for the image graph
    */
  def communityStructure(size: Int): Array[Array[Boolean]] = Array.tabulate(size, size)((i,j) => i==j)

  def randomWithImageEqualClusterSize(M: Array[Array[Boolean]], n: Int, rng: Random = new Random()): Blockmodel = {
    val k = M.length
    val C = rng.shuffle( Vector.tabulate(n)(i => i % k) ).toArray
    new Blockmodel(C, M)
  }

  def fromImageEqualClusterSize(M: Array[Array[Boolean]], n: Int): Blockmodel = {
    val k = M.length
    val C = Array.tabulate(n)(i => i % k)
    new Blockmodel(C, M)
  }

  def random(g: Digraph, k: Int, rng: Random): Blockmodel = {
    val vertices = rng.shuffle(Array.tabulate(g.n)(identity))
    val C: Array[Int] = Array.tabulate(g.n)(i => vertices(i)%k)
    val cluster = Array.tabulate(k)(c => vertices.filter(C(_)==c))
    val M: Array[Array[Boolean]] = Array.tabulate(k,k)((c,d) => {
      val nb1 = cluster(c).zip(cluster(d)).count{case (i,j) => g(i)(j)}
      // M(c)(d) is true if there are more ones than not
      nb1 > (cluster(c).length * cluster(d).length)-nb1
    })

    new Blockmodel(C, M)
  }

  def fromToulbar2Solution(sol: String): Blockmodel = {
    // tokens are of the for varname=value, separated by spaces. for example x0=4 x1=0 x2=4 x3=4
    val tokens = sol.split(" ")
      .filter(_.contains("=")) //remove empty tokens
      .map(_.split("=")) // split on =
      .map(a => (a(0), a(1).toInt)) // transform value to int

    val k = 1 + tokens.map(_._2).max
    val n = 1 + tokens.map(_._1).filter(_.startsWith("x")).map(_.substring(1).toInt).max

    val M = Array.fill(k,k)(false)
    val C = Array.fill(n)(k)

    for((name, value) <- tokens) {
      if (name.startsWith("M")) {
        println(s"processing $name = $value")
        val x = name.split("_")
        val cluster1 = x(1).toInt
        val cluster2 = x(2).toInt
        M(cluster1)(cluster2) = value > 0
      }
      else if(name.startsWith("x")) {
        println(s"processing $name = $value")
        val i = name.substring(1).toInt
        C(i) = value
      }
    }
    new Blockmodel(C, M)
  }

}