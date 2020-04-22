package blockmodel.utils

import java.io.{File, PrintWriter}

import blockmodel.utils.Matrix._

import scala.collection.mutable
import scala.util.Random

class Digraph(val adjacencyMatrix: Array[Array[Boolean]], val names: Int => String = _.toString) {
  val n: Int = adjacencyMatrix.length // number of nodes
  val e: Int = adjacencyMatrix.flatten.count(identity) // number of edges
  for (row <- adjacencyMatrix if row.length != n)
    throw new IllegalArgumentException("The provided adjacency matrix is not square")

  def apply(i: Int): Array[Boolean] = adjacencyMatrix(i)

  def apply(i: Int, j: Int): Boolean = adjacencyMatrix(i)(j)

  override def toString: String = {
    adjacencyMatrix.mapCells(if (_) "1" else "·").toStringMatrix
  }

  def isSymmetrical: Boolean = {
    var res = true
    var i = 0
    while (res && i < n) {
      var j = 0
      while (res && j <= i) {
        res = adjacencyMatrix(i)(j) == adjacencyMatrix(j)(i)
        j += 1
      }
      i += 1
    }
    res
  }

  def saveTSV(filename: String): Unit = {
    val res = new StringBuilder()
    res append (0 until n map names mkString "\t")
    res append "\n"
    for (i <- 0 until n) {
      res append (0 until n map adjacencyMatrix(i) map (if(_) 1 else 0) mkString "\t")
      res append "\n"
    }
    val pw = new PrintWriter(new File(filename))
    pw.write(res.toString())
    pw.close
  }
}

object Digraph {
  def fromTSV(filename: String): Digraph = {
    val file = scala.io.Source.fromFile(filename)
    val lines = file.getLines()
    val names = lines.next().split("\t")
    val matrix = lines.toArray.map(line => line.split("\t").map(cell => cell == "1"))
    if (names.length != matrix.length) throw new IllegalArgumentException("The number of names does not correspond to" +
      " the size of the matrix for file "+filename)
    file.close()
    new Digraph(matrix, names)
  }
  def fromTSV(graphFile: java.io.File): Digraph = {
    val file = scala.io.Source.fromFile(graphFile)
    val lines = file.getLines()
    val names = lines.next().split("\t")
    val matrix = lines.toArray.map(line => line.split("\t").map(cell => cell == "1"))
    if (names.length != matrix.length) throw new IllegalArgumentException("The number of names does not correspond to" +
      " the size of the matrix for file "+graphFile.toString)
    file.close()
    new Digraph(matrix, names)
  }
  def fromNumericalTSV(filename: String, binarisation: Double=>Boolean): Digraph = {
    val file = scala.io.Source.fromFile(filename)
    val lines = file.getLines()
    val names = lines.next().split("\t")
    val matrix = lines.toArray.map(line => line.split("\t").map(s => binarisation(s.toDouble)))
    if (names.length != matrix.length) throw new IllegalArgumentException("The number of names does not correspond to" +
      " the size of the matrix for file "+filename)
    file.close()
    new Digraph(matrix, names)
  }

  def random(size: Int, rng: Random): Digraph = {
    val matrix = Array.fill(size, size)(rng.nextBoolean())
    new Digraph(matrix)
  }

  /**
    * generate a random digraph with the structure dictated by the provided image matrix M. The cluster will all have
    * equal size (±1). The vertices are randomly assigned to the clusters, so the matrix looks random.
    * Additionally, a certain proportion of error can be added to the matrix. For example, an error of 0.2 will invert
    * the value of 20% of the entries of the generated matrix.
    * @param M the image matrix giving the structure that this digraph must follow
    * @param n the number of vertices in the generated graph
    * @param rng the random number generator to be used
    * @param error the proportion of entries of the matrix to be flipped. Must be between 0.0 and 1.0
    * @return a digraph matching the image matrix M with the specified proportion of error
    */
  def randomWithImage(M: Array[Array[Boolean]], n: Int, rng: Random = new Random(), error: Double = 0.0): Digraph = {
    val b = M.length
    val matrix = Array.fill(n,n)(false)

    // fill matrix from blockmodel
    val N = (0 until n).toVector
    val σ = rng.shuffle(N) // permutation
    for (i <- 0 until n; j <- 0 until n) {
      matrix(i)(j) = M( σ(i)%b )( σ(j)%b )
    }

    val wrongIndices = mutable.Set[(Int,Int)]()
    while (wrongIndices.size < n*n*error) wrongIndices.add((rng.nextInt(n), rng.nextInt(n)))
    for ((i,j) <- wrongIndices) matrix(i)(j) = !matrix(i)(j)

    new Digraph(matrix)
  }

  /*
  def fromBlockmodelWithError(bm: blockmodel.Blockmodel, rng: Random = new Random(), error: Double = 0.0): Digraph = {
    val matrix = fromBlockmodel(bm)

    val wrongIndices = mutable.Set[(Int,Int)]()
    while (wrongIndices.size < bm.n*bm.n*error) wrongIndices.add((rng.nextInt(bm.n), rng.nextInt(bm.n)))
    for ((i,j) <- wrongIndices) matrix(i)(j) = !matrix(i)(j)

    matrix
  }

  def fromBlockmodel(bm: blockmodel.Blockmodel): Digraph =
    new Digraph(Array.tabulate(bm.n,bm.n)((i,j) => bm.M( bm.C(i) )( bm.C(j) )))
  */
}
