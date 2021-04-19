package problem4

object Problem4a {

  val required = Vector("byr", "eyr", "pid", "hgt", "iyr", "ecl", "hcl")

  def loadInput(filename: String): List[String] = {
    utils.Utils.loadInput(filename).toList
  }

  def countValid(input: List[String]): Int = {
    def traverse(input: List[String], actualData: List[String]): Int = input match {
      case Nil => 0
      case ""+:rest => process(actualData) + traverse(rest, List())
      case str +: rest => traverse(rest, actualData :+ str)
    }
    traverse(input, List())
  }

  def process(actualData: List[String]): Int = {
    val dataString = actualData.mkString(" ")
    val pairs = dataString.split(' ')
    val tokens = pairs.map[String]( pair => pair.split(':')(0))
    if (required.forall(req => tokens.contains(req))) 1 else 0
  }


  def main(args: Array[String]): Unit = {
    val input = loadInput("input4a.txt")
    println(s"problem4a result = ${countValid(input)}")
  }
}
