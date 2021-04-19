package problem4

object Problem4b {
  val required = Vector("byr", "eyr", "pid", "hgt", "iyr", "ecl", "hcl")

  def loadInput(filename: String): List[String] = {
    utils.Utils.loadInput(filename).toList
  }

  def validPassports(input: List[String]): List[List[String]] = {
    def traverse(input: List[String], actualData: List[String]): List[List[String]] = input match {
      case Nil => Nil
      case ""+:rest => process(actualData) :: traverse(rest, List())
      case str +: rest => traverse(rest, actualData :+ str)
    }
    traverse(input, List()).filter(_.nonEmpty)
  }
  def process(actualData: List[String]): List[String] = {
    val dataString = actualData.mkString(" ")
    val pairs = dataString.split(' ')
    val tokens = pairs.map[String]( pair => pair.split(':')(0))
    if (required.forall(req => tokens.contains(req))) actualData else Nil
  }

  def validValues(passports: List[List[String]]): Int = passports match {
    case Nil => 0
    case pass +: rest => check(pass) + validValues(rest)
  }
  def check(passport: List[String]): Int = {
    val pairs = passport.mkString(" ").split(' ').map(_.split(':'))
    def classify(pair: List[String]): Boolean = {
      val attrName = pair(0).trim
      val value = pair(1).trim

      if(attrName == "byr")  value.toInt >= 1920 && value.toInt <= 2002
      else if(attrName == "iyr") value.toInt >= 2010 && value.toInt <= 2020
      else if(attrName == "eyr") value.toInt >= 2020 && value.toInt <= 2030
      else if(attrName == "hgt") {
        val (height, metric) = value.splitAt(value.length - 2)
        if(metric == "in") height.toInt >= 59 && height.toInt <= 76
        else if (metric == "cm") height.toInt >= 150 && height.toInt <= 193
        else false
      }
      else if(attrName == "hcl")
        value.length == 7 && value.drop(1).forall(ch => (ch >= 'a' && ch <= 'f') || (ch >= '0' && ch <= '9'))
      else if(attrName == "ecl"){
        Array("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)

      }else if(attrName == "pid") value.length == 9
      else if(attrName == "cid"){
        true
      }else {
        println(attrName, "nic")
        false
      }
    }
    if (pairs.forall(pair => classify(pair.toList))) 1 else 0

  }




  def main(args: Array[String]): Unit = {
    val input = loadInput("input4a.txt")
    val validPassports = Problem4b.validPassports(input)
    println(validValues(validPassports))
  }
}
