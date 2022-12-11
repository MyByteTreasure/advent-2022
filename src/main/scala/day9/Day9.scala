package be.sexysmidt
package day9

import util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

@main def day9(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day9\\input.txt")
  val text = input.mkString

  val moves = text.split("\n")
    .map(el => el.trim)
    .map(el => el.split(" "))
    .map(el => (el.head, el.last.toInt))

  println(moves.toVector)

  //PART 1
  val startPosTwo =  Array(createStartPos(2))

  val tailPositionsTwo = moveAround(moves, startPosTwo)
  val uniquePositionsTwo = tailPositionsTwo.toSet.size

  println("Amount of unique positions with Rope Length 2 => " + uniquePositionsTwo)



  //PART 2
  val startPosNine = Array(createStartPos(10))
  val tailPositionsNine = moveAround(moves, startPosNine)
  val uniquePositionsNine = tailPositionsNine.toSet.size
  println(tailPositionsNine.distinct.toVector)
  println("Amount of unique positions with rope length 9 => " + uniquePositionsNine)


}

@tailrec
def moveAround(moves: Array[(String, Int)], ropePos: Array[Array[(Int, Int)]]): Array[(Int, Int)] = {
  if (moves.isEmpty) {
    val ropeLength = ropePos.head.length
    ropePos.flatMap(el => el.zipWithIndex).groupBy(el => el._2)(ropeLength - 1).map(el => el._1)
  } else {
    val newPositions = move(moves.head, ropePos)
    moveAround(moves.tail, newPositions)
  }
}

@tailrec
def move(instuction: (String, Int), ropePos: Array[Array[(Int, Int)]]):  Array[Array[(Int, Int)]] = {
  if (instuction._2 == 0) {
    ropePos
  } else {
    val updatedHeadPos = moveHead(instuction._1, ropePos.last.head)
    val updatedPos = moveTailKnots(Array(updatedHeadPos), ropePos.last.tail)
    move(instuction.copy(_2 = instuction._2 - 1), ropePos.appended(updatedPos))
  }
}

def moveHead(direction: String, pos: (Int, Int)) = (direction: @switch) match {
  case "U"  => pos.copy(_2 = pos._2 + 1)
  case "D"  => pos.copy(_2 = pos._2 - 1)
  case "L"  => pos.copy(_1 = pos._1 - 1)
  case "R"  => pos.copy(_1 = pos._1 + 1)
}

@tailrec
def moveTailKnots(newPos: Array[(Int, Int)], oldPos: Array[(Int, Int)]): Array[(Int, Int)] = {
  if (oldPos.isEmpty) {
    newPos
  } else {
    val movedKnot = moveTail(newPos.last, oldPos.head)
    moveTailKnots(newPos.appended(movedKnot), oldPos.tail)
  }

}
def moveTail(headPos: (Int, Int), tailPos: (Int, Int)): (Int, Int) = {

  val diff = (headPos._1 - tailPos._1, headPos._2 - tailPos._2)
  if (diff == (0, 0) || abs(diff) == (0, 1) || abs(diff) == (1, 0) || abs(diff) == (1, 1)) {
    tailPos
  } else if (abs(diff) == (0, 2)) {
    tailPos.copy(_2 =  tailPos._2 + (diff._2 / 2))
  } else if (abs(diff) == (2, 0)) {
    tailPos.copy(_1 =  tailPos._1  + (diff._1 / 2))
  } else if (abs(diff) == (2, 1)) {
    (tailPos._1  + (diff._1 / 2), tailPos._2 + diff._2)
  } else if (abs(diff) == (1, 2)) {
    (tailPos._1  + diff._1, tailPos._2 + (diff._2 / 2))
  } else {
    (tailPos._1  + (diff._1 / 2), tailPos._2 + (diff._2 / 2))
  }
}

def abs(pos: (Int, Int)): (Int, Int) = {
  (pos._1.abs, pos._2.abs)
}

def createStartPos(amountOfKnots: Int): Array[(Int, Int)] = {
  Array.fill(amountOfKnots){(0, 0)}
}
