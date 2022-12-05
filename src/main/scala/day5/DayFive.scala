package be.sexysmidt
package day5

import be.sexysmidt.util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

val rock = "Rock"
val paper = "Paper"
val scissors = "Scissors"

val map = Map(
  rock -> (scissors, paper),
  paper -> (rock, scissors),
  scissors -> (paper, rock),
)

@main def calculateScore(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day5\\input.txt")
  val text = input.mkString

  val baseInput = text.split(RegexHelper.emptyLineRegex())
    .filter(StringHelper.checkNotBlank)

  val stackString = baseInput.head
  val movesString = baseInput.last

  //PART 1

  val capitalRegex = Pattern.compile("\\w")

  val initialStackMap = stackString
    .split("\n")
    .map(el => el.sliding(4, 4))
    .map(el => el.map(cargo => getCargoLetter(cargo, capitalRegex)))
    .flatMap(el => el.zipWithIndex)
    .groupBy(el => el._2)
    .map(el => (el._1 + 1, el._2.map(el => el._1).toVector.filter(StringHelper.checkNotBlank).reverse.tail.reverse))

  val movesInputSplit = movesString
   .split("\n")
   .filter(StringHelper.checkNotBlank)
   .map(el => el.trim)

  println(movesInputSplit.last)

  val pattern = Pattern.compile("move (\\d+) from (\\d+) to (\\d+)")

  val movesList = movesInputSplit
    .map(el => createMovesTuple(el, pattern))

  val map9000 = crane9000(initialStackMap, movesList)

  val topCargo = map9000.toVector.sortBy(el => el._1).flatMap(el => el._2.take(1)).mkString

  println("These containers end on top with 9000 crane=> "  + topCargo)

  //PART 2

  val map9001 = crane9001(initialStackMap, movesList)

  val topCargoTwo = map9001.toVector.sortBy(el => el._1).flatMap(el => el._2.take(1)).mkString

  println("These containers end on top with 9001 crane => " + topCargoTwo)

}

def moveCargoPerInstruction(map: Map[Int, Vector[String]],
                            instuction: (Int, Int, Int),
                            craneFunction: (Vector[String], Vector[String], Int) => (Vector[String], Vector[String])
                           ): Map[Int, Vector[String]] = {
  val stackA = map.get(instuction._2)
  val stackB = map.get(instuction._3)

  val newStacks = craneFunction(stackA.get, stackB.get, instuction._1)

  map
    .updated(instuction._2, newStacks._1)
    .updated(instuction._3, newStacks._2)
}

@tailrec
def moveCargo9000Crane(stackA: Vector[String], stackB: Vector[String], amount: Int): (Vector[String], Vector[String]) = {
  if (amount == 0) {
    (stackA, stackB)
  } else {
    moveCargo9000Crane(stackA.tail, stackB.prepended(stackA.head), amount -1)
  }
}

def moveCargo9001Crane(stackA: Vector[String], stackB: Vector[String], amount: Int): (Vector[String], Vector[String]) = {

  (stackA.takeRight(stackA.length - amount), stackB.prependedAll(stackA.take(amount)))
}

def createMovesTuple(text: String, regex: Pattern): (Int, Int, Int) = {
  val matcher = regex.matcher(text)
  matcher.matches()
  (matcher.group(1).toInt, matcher.group(2).toInt, matcher.group(3).toInt)
}

def getCargoLetter(text: String, regex: Pattern): String = {
  val matcher = regex.matcher(text)
  if (matcher.find()) {
    matcher.group()
  } else
    " "
}

@tailrec
def crane9000(map: Map[Int, Vector[String]], instuctions: Array[(Int, Int, Int)]): Map[Int, Vector[String]] = {
  if (instuctions.isEmpty) {
    map
    } else {
    crane9000(moveCargoPerInstruction(map, instuctions.head, moveCargo9000Crane), instuctions.tail)
  }

}

@tailrec
def crane9001(map: Map[Int, Vector[String]], instuctions: Array[(Int, Int, Int)]): Map[Int, Vector[String]] = {
  if (instuctions.isEmpty) {
    map
  } else {
    crane9001(moveCargoPerInstruction(map, instuctions.head, moveCargo9001Crane), instuctions.tail)
  }

}
