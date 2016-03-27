package photo

import java.io.File


object FileSizeCalculator  extends App {

  val rootPath = "/home/pavlo/Pictures/Photo"
  private val MEGABYTE = 1024 * 1024

  def fileList(dir: File): Array[File] = {
    val dirFiles: Array[File] = dir.listFiles()
    dirFiles ++ dirFiles.filter(_.isDirectory).flatMap(fileList)
  }

  def groupByFileType(files: Array[File]) = {
    files.filter(!_.isDirectory).groupBy(_.getName.split("\\.").last.toUpperCase)
  }

  private val fileType: Map[String, Array[File]] = groupByFileType(fileList(new File(rootPath)))
  private val stringToLong: Map[String, Long] = fileType.map(
    ext => (ext._1, ext._2.foldLeft(0L)((sum, file) => sum + file.length()))
  )
  val extToMb = stringToLong.mapValues(_ / MEGABYTE)
  println(extToMb)

}
