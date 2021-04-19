package problem5

object Problem5a {


  def findSeatId(seatCode: String): Int = {
    val (rowCode, colCode) = seatCode.splitAt(7)
    val rowId = findPartialId(0, 127, rowCode)
    val colId = findPartialId(0, 7, colCode)
    rowId * 8 + colId
  }

  def findPartialId(lower: Int, upper: Int, code: String): Int = {
    val (next, rest) = code.splitAt(1)
    next match {
      case "B"|"R" =>{
        if(rest.isEmpty) upper
        else findPartialId((lower + upper + 1) /2, upper, rest)
      }
      case "F"|"L" => {
        if(rest.isEmpty) lower
        else findPartialId(lower, (lower + upper) / 2, rest)
      }
    }
  }


  def getSeatIds(input: List[String]): List[Int] = input match {
    case Nil => Nil
    case code +: rest => findSeatId(code) +: getSeatIds(rest)
  }

  def findMySeat(seatIds: List[Int]): Int = {
    val sortedIds = seatIds.sorted
    val min = sortedIds(0)
    sortedIds.zipWithIndex.find{case (id, index) => id - index != min }.get._1 - 1
  }


  def main(args: Array[String]): Unit = {
    val input = utils.Utils.loadInput("input5a.txt").toList
    val seatIds = getSeatIds(input)
    println(s"maximum seat: ${seatIds.max}")
    println(s"my seat is: ${findMySeat(seatIds)}")
  }
}
