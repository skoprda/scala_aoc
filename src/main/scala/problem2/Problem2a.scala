package problem2

import utils.Utils
case class Password(from: Int, to: Int, char: Char, passwd: String)  {
  def isCorrect(): Boolean = {
    val count: Int = passwd.groupBy(c => c == char).get(true).fold(0)(s => s.length)
    count <= to && count >= from
  }
}

object Problem2a {

    def parseInput(filename: String): List[Password2] = {
      val lines = Utils.loadInput(filename)
      lines.map[Password2](parseLine).toList
    }

    def parseLine(line: String): Password2= {
      val tokens = line.split(' ')
      val fromTo = tokens(0).split('-')
      val from = fromTo(0).toInt
      val to = fromTo(1).toInt
      val char = tokens(1).charAt(0)
      val password = tokens(2)
      Password2(from, to, char, password)
    }

  def correctCount(parsedInput: List[Password2]): Int = {
    parsedInput.groupBy(_.isCorrect()).get(true).fold(0)(l => l.length)
  }

  def main(args: Array[String]): Unit = {
    val parsedInput = parseInput("input2a.txt")
    print(parsedInput)
    val result = correctCount(parsedInput)
  }
}
