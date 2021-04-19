package problem3

import utils.Utils

object Problem3 {

  def loadInput(filename: String): Vector[String] ={
    Utils.loadInput(filename)
  }

  def treesCount(input: Vector[String], right: Int, down: Int): Int = {
    def traverse(row: Int, col: Int): Int = {
      if(row >= input.length) 0
      else {
        val str = input(row)
        if (str.charAt(col) == '#') traverse(row + down, (col + right) % str.length) + 1
        else traverse(row + down, (col + right) % str.length)
      }
    }
    traverse(0,0)
  }

  def main(args: Array[String]): Unit = {
    val input = loadInput("input3a.txt")

    println(s"problem 3a = ${treesCount(input, 3,1)}")

    val slope1_1 = treesCount(input, 1, 1)
    val slope3_1 = treesCount(input, 3, 1)
    val slope5_1 = treesCount(input, 5, 1)
    val slope7_1 = treesCount(input, 7, 1)
    val slope1_2 = treesCount(input, 1, 2)
    val result = slope1_1 * slope3_1 * slope5_1 * slope7_1 * slope1_2
    println(s"problem 3b = ${result}")
  }
}
