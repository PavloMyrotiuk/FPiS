package matrix

case class Matrix(repr: Array[Array[Double]]) {

  def row(index: Int) = repr(index)

  def column(index: Int) = repr.map(_ (index))

  lazy val rowCount = repr.size
  lazy val columnCount = if (repr.size == 0) 0 else repr(0).size

  override def toString = repr.mkString("\n", "|", "\n")
}



object Index extends App{
  private val matrixA: Matrix = Matrix(Array(Array(2, 3, 4)))
  private val matrixB: Matrix = Matrix(Array(Array(1), Array(1), Array(1)))
  println(matrixA)
  println(matrixB)
}