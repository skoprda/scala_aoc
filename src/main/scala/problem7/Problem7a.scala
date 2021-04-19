package problem7

object Problem7a {

  def parseInput(input: List[String]): List[Bag] = {
    input.map(parseLine(_))
  }
  def parseLine(line: String): Bag = {
    val nameInnerBags = line.split("contain")
    val incorrectName = nameInnerBags(0).trim
    val name = if(incorrectName.endsWith("s")) incorrectName.take(incorrectName.length - 1) else incorrectName
    val innerBags = nameInnerBags(1).split(", ").map(_.splitAt(2))
    if(innerBags(0)._1.trim == "n") Bag(name, Map())
    else {
      val parsedInnerBags = innerBags.map { case (count, innerName) => {
        if (innerName.endsWith("s.")) (innerName.take(innerName.length - 2).trim, count.trim.toInt)
        else if (!innerName.endsWith("g")) (innerName.take(innerName.length - 1).trim, count.trim.toInt)
        else (innerName.trim, count.trim.toInt)
      }
      }.toMap
      Bag(name, parsedInnerBags)
    }
  }

  def canContainShinyGold(parsedInput: List[Bag], bagsThatCan: List[String]=List("shiny gold bag"), searched: List[String]): Int = {
    if(bagsThatCan.isEmpty) 0
    else{
      val newBagsThatCan = parsedInput.filter(bag => bagsThatCan.find(b => bag.innerBags.get(b).nonEmpty).nonEmpty).map(_.name)
      val filteredNewBags = newBagsThatCan.filter(name => !searched.contains(name))
      newBagsThatCan.length + canContainShinyGold(parsedInput, filteredNewBags, searched ::: bagsThatCan)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = utils.Utils.loadInput("input7a.txt").toList
    val parsedInput = parseInput(input)
    println(canContainShinyGold(parsedInput, searched = List()))

  }
}

case class Bag(name: String, innerBags: Map[String, Int])
