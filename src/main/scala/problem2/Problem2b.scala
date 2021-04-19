package problem2


import utils.Utils

case class Password2(first: Int, second: Int, char: Char, passwd: String)  {
  def isCorrect(): Boolean = {
      passwd.charAt(first-1) == char && passwd.charAt(second-1) != char ||
        passwd.charAt(first-1) != char && passwd.charAt(second-1) == char

  }
}

object Problem2b {

  def parseInput(filename: String): List[Password2] = {
    val lines = Utils.loadInput(filename)
    lines.map[Password2](parseLine).toList
  }

  def parseLine(line: String): Password2= {
    val tokens = line.split(' ')
    val first_second = tokens(0).split('-')
    val first = first_second(0).toInt
    val second = first_second(1).toInt
    val char = tokens(1).charAt(0)
    val password = tokens(2)
    Password2(first, second, char, password)
  }

  def correctCount(parsedInput: List[Password2]): Int = {
    parsedInput.groupBy(_.isCorrect()).get(true).fold(0)(l => l.length)
  }

  def main(args: Array[String]): Unit = {
    val parsedInput = parseInput("input2a.txt")
    val result = correctCount(parsedInput)
    print(result)
  }
}

