package problem6

object Problem6b {

  def countYeses(input: Vector[String]): Int = {
    val groupsString = input.mkString(",").split(",,")
    val groups = groupsString.map(_.split(','))
    groups.map(groupCountYeses(_)).sum
  }
  def groupCountYeses(group: Array[String]): Int = {
    group.map(_.toSet).reduce(_ intersect _).size
  }

  def main(args: Array[String]): Unit = {
    val input = utils.Utils.loadInput("input6a.txt")
    println(s"yes answers: ${countYeses(input)}")
  }
}
