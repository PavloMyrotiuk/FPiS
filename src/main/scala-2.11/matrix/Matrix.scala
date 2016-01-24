package matrix

import java.util.concurrent.{Future, Callable, Executors}

case class Matrix(repr: Array[Array[Double]]) {

  def row(index: Int) = repr(index)

  def column(index: Int) = repr.map(_ (index))

  lazy val rowCount = repr.size
  lazy val columnCount = if (repr.size == 0) 0 else repr(0).size

  override def toString = repr.foldLeft("Matrix: {")((acc, row) => acc + row.mkString("\n", "|", "")) + "\n}"
}

object Matrix {

  def multiply(matrixA: Matrix, matrixB: Matrix)(implicit strategy: TreadStrategy): Matrix = {
    assert(matrixA.columnCount == matrixB.rowCount)

    val buffer: Array[Any] = new Array(matrixA.rowCount)
    val resultMap: Array[Array[() => Double]] = buffer.map(_ => new Array[() => Double](matrixB.columnCount))

    for (i <- 0 until matrixA.rowCount; j <- 0 until matrixB.columnCount)
      yield resultMap(i)(j) = strategy.calculate(() => multiplyColumnByRow(matrixA.row(i), matrixB.column(j)))

    Matrix(resultMap.map(_.map(_ ())))
  }

  def multiplyColumnByRow(row: Array[Double], column: Array[Double]): Double = {
    column.zip(row).foldLeft(0.0)((acc, pair) => acc + pair._1 * pair._2)
  }
}

trait TreadStrategy {
  def calculate[B](func: () => B): () => B
}

object SingleThreadStrategy extends TreadStrategy {
  override def calculate[B](func: () => B) = func
}

object ThreadPoolStrategy extends TreadStrategy {
  val pool = Executors.newFixedThreadPool(java.lang.Runtime.getRuntime.availableProcessors)

  override def calculate[B](func: () => B) = {
    val future: Future[B] = pool.submit(new Callable[B] {
      override def call(): B = {
        println(" Executing function on thread: " + Thread.currentThread().getName)
        func()
      }
    })
    () => future.get()
  }
}

object Index extends App {
  private val matrixA: Matrix = Matrix(Array(Array(2, 3, 4), Array(20, 30, 40)))
  private val matrixB: Matrix = Matrix(Array(Array(1, 2), Array(1, 2), Array(1, 2)))
  println(matrixA)
  println(matrixB)

  implicit val sts = ThreadPoolStrategy
  println(Matrix.multiply(matrixA, matrixB))

}