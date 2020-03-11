package blockmodel.utils

import scala.reflect.ClassTag

/**
  * a bunch of useful methods for matrices
  */
class Matrix[T:ClassTag](val array: Array[Array[T]]) {

  def rows: Array[Array[T]] = array
  def cols: Array[Array[T]] = Array.tabulate[T](nbCols, nbRows)((i,j) => array(j)(i))

  def cells: Iterable[T] = array.flatten.toIndexedSeq

  val nbRows: Int = array.length
  val n: Int = nbRows

  val nbCols: Int = if (array.length == 0) 0 else array(0).length
  val m: Int = nbCols

  def sameMatrix(that: Matrix[T]): Boolean =
    this.n == that.n && this.m == that.m && {
      var i = 0
      while (i < n && (this.array(i) sameElements that.array(i))) i+= 1
      i == n
    }

  def isSquare: Boolean = nbRows == nbCols

  def transpose: Matrix[T] = Array.tabulate(nbCols,nbRows)((i,j) => array(j)(i))

  def elementWise[U:ClassTag, V:ClassTag](f: (T,U) => V)(m: Matrix[U]): Matrix[V] =
    Array.tabulate(nbRows, nbCols)((i,j) => f(array(i)(j), m.array(i)(j)))

  /**
    * map every cell in the matrix with function f
    *
    * @param f the function to be applied to each cell of the matrix
    * @tparam U the resulting type of the matrix after the map operation
    * @return a matrix of the same dimension where element m'(i)(j) is f(m(i)(j))
    */
  def mapCells[U](f: T => U)(implicit evidence: ClassTag[U]): Matrix[U] =
    Array.tabulate(nbRows, nbCols)((i, j) => f(array(i)(j)))


  def trim(newn: Int, newm: Int): Matrix[T] =
    Array.tabulate(newn, newm)((i,j) => array(i)(j))


  def reorder(σn: Int => Int, σm: Int => Int): Matrix[T] =
    Array.tabulate(n,m)((i,j) => array( σn(i) )( σm(j) ))

  def toStringMatrix: String = {
    if (cells.isEmpty) ""
    else {
      val cellSize = cells.map(_.toString.length).max
      val fmt = s"%${cellSize}s"
      array.map(row => row.map(cell => fmt.format(cell)).mkString(" ")).mkString("\n")
    }
  }
}

class IntMatrix(array: Array[Array[Int]]) extends Matrix[Int](array) {
  def this(matrix: Matrix[Int]) = this(matrix.array)
  def +(x: IntMatrix): IntMatrix = new IntMatrix(elementWise[Int,Int](_+_)(x))
  def -(x: IntMatrix): IntMatrix = new IntMatrix(elementWise[Int,Int](_-_)(x))
  def *(i: Int): IntMatrix = new IntMatrix(mapCells(_*i))
  def hadamardProduct(x: IntMatrix): IntMatrix =
    new IntMatrix(elementWise[Int,Int](_*_)(x))
  /*def multiply(x: IntMatrix): IntMatrix =
    new IntMatrix(Array.tabulate(nbRows, x.nbCols)((i,j) =>
      Σi(0,nbCols-1)(k => array(i)(k) * x.array(k)(j))))
   */
}

object Matrix {
  implicit def matrixToArray[T:ClassTag](m: Matrix[T]): Array[Array[T]] = m.array
  implicit def ArrayToMatrix[T:ClassTag](a: Array[Array[T]]): Matrix[T] = new Matrix[T](a)
}