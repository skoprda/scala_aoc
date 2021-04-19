package problem6

object Problem6a {


  def countYeses(input: Vector[String]): Int = {
    val groupsString = input.mkString(",").split(",,")
    val groups = groupsString.map(_.split(','))
    groups.map(groupCountYeses(_)).sum
  }
  def groupCountYeses(group: Array[String]): Int = {
    group.reduce(_ + _).toSet.size
  }

  def main(args: Array[String]): Unit = {
    val input = utils.Utils.loadInput("input6a.txt")
    println(s"yes answers: ${countYeses(input)}")
  }
}
