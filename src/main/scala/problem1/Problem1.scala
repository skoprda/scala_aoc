package problem1

import utils.Utils

import scala.io.Source

object Problem1{

  def calculateResult1a(lines: Vector[Int]): Int ={
    val sortedLines = lines.sorted
    def meetInTheMiddle(left: Int, right: Int): Int = {
      if(left == right) -1
      else {
        val sum = sortedLines(left) + sortedLines(right)
        if (sum == 2020) sortedLines(left) * sortedLines(right)
        else if (sum < 2020) meetInTheMiddle(left + 1, right)
        else meetInTheMiddle(left, right - 1)
      }
    }
    meetInTheMiddle(0, sortedLines.length-1)
  }

  def calculateResult1b(lines: Vector[Int]): Int ={
    val sortedLines = lines.sorted
    def findTriple(first: Int, second: Int): Int = {
      if (first == sortedLines.length) -1
      else {
        if (second == sortedLines.length) findTriple(first + 1, first + 2)
        else {
          val fstValue = sortedLines(first)
          val sndValue = sortedLines(second)
          val sum = fstValue + sndValue

          def binarySearch(lower: Int, upper: Int): Int = {
            if (lower > upper) -1
            else {
              val middle = (lower + upper) / 2
              val midValue = sortedLines(middle)
              if (midValue + sum == 2020) midValue * fstValue * sndValue
              else if (midValue + sum < 2020) binarySearch(middle + 1, upper)
              else binarySearch(lower, middle - 1)
            }
          }

          val bsResult = binarySearch(0, sortedLines.length - 1)
          if (bsResult == -1) findTriple(first, second + 1)
          else bsResult
        }
      }
    }
    findTriple(0,1)
  }

  def main(args: Array[String]): Unit = {
    val lines = Utils.loadInput("input1a.txt")
    val linesInt = lines.map(_.toInt)
    val result1 = calculateResult1a(linesInt)
    val result2 = calculateResult1b(linesInt)
    println(result1)
    println(result2)
  }


}
