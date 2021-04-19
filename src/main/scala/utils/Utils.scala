package utils

import scala.io.Source

object Utils {

  def loadInput(filename: String): Vector[String] = {
    val bufferedSource = Source.fromFile(filename)
    var lines = Vector[String]()
    for(line <- bufferedSource.getLines()){
      lines = lines :+ line
    }
    lines
  }
}
