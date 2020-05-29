package blockmodel

import java.awt.{Color, Image}
import java.awt.image.BufferedImage

import utils.Digraph
import utils.Matrix._

import scala.util.Random

/**
  * class representing a block model, with its clusters and its image matrix.
  * @param C the cluster index for every vertex in the graph. vertex v is in cluster i <=> C(v)=i
  * @param M image matrix for the block model
  */
class Blockmodel(val C: Array[Int], val M: Array[Array[Boolean]]){
  val n = C.length // number of vertices
  val k = M.length // number of clusters
  for (row <- M if row.length != k) throw new IllegalArgumentException("M is not a square matrix")
  for (c <- C if c < 0 || c >= k) throw new IllegalArgumentException(s"$c cannot be a vertex label if k = $k")

  def Σij(ito: Int, jto: Int)(f: (Int, Int) => Int): Int = {
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

  def cost(g: Digraph): Int = Σij(n,n)((i,j) => if(g(i)(j) == M( C(i) )( C(j) )) 0 else 1)
  //def mdl(g: Digraph): Int =(n*k)+(k*k)+(2*cost(g))
  //def mdl(g: Digraph): Int =(n*k)+(k*k)+(cost(g) * 2 * math.log(n)/math.log(2)).toInt

  def costOfVertex(v: Int, g: Digraph): Int = {
    val row = (0 until n).foldRight(0){case (i,sum) => sum  + (if(g(v)(i) == M( C(v) )( C(i) )) 0 else 1) }
    val col = (0 until n).foldRight(0){case (i, sum) => sum + (if(g(i)(v) == M( C(i) )( C(v) )) 0 else 1) }
    row + col
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
        result append nameFmt.format(g.names(i)+" "+(i)) + "┃"
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

  def toImageGrouped(g: Digraph): BufferedImage = {
    val size = g.n + k
    val img = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
    val white = Color.WHITE.getRGB
    val red   = Color.RED.getRGB
    val black = Color.BLACK.getRGB

    val groupedVertices = (0 to C.max).map(r => C.indices.filter(C(_) == r))
    var pixels = List[Int]()
    for (group <- groupedVertices) {
      // horizontal separator
      pixels ++= List.fill(size)(red)
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
      img.setRGB(x, y, pixels(y+x*size))
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

}