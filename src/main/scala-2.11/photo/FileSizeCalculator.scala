package photo

import java.io.File

object FileSizeCalculator extends App {

  val rootPath = "/home/pavlo/Pictures/Photo"
  private val MEGABYTE = 1024 * 1024

  def fileList(dir: File): Array[File] = {
    val dirFiles: Array[File] = dir.listFiles()
    dirFiles ++ dirFiles.filter(_.isDirectory).flatMap(fileList)
  }

  def analyzeSize(dirName: String) = {

    def groupByFileType(files: Array[File]) = {
      files.filter(!_.isDirectory).groupBy(_.getName.split("\\.").last.toUpperCase)
    }

    groupByFileType(fileList(new File(rootPath))).map(
      ext => (ext._1, ext._2.foldLeft(0L)((sum, file) => sum + file.length()))
    ).mapValues(_ / MEGABYTE)
  }

  def locateFiles(fileNames: List[String], parentDir: String): List[String] =
    fileList(new File(parentDir)).toList.filter(file => fileNames.contains(file.getName)).map(_.getPath)

  println(locateFiles(List("IMG_3929.JPG",
    "IMG_2983.JPG",
    "IMG_3008.JPG",
    "IMG_9565.JPG",
    "IMG_8954.JPG",
    "IMG_9376.JPG",
    "IMG_9901.JPG",
    "IMG_4388.JPG",
    "MVI_5334.MOV",
    "IMG_7202.JPG",
    "456.jpg",
    "736.jpg",
    "741.jpg",
    "753.jpg",
    "q53.jpg",
    "020520111683.jpg"), rootPath).mkString("\n"))


}
