package be.sexysmidt
package day10

import util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

@main def day9(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day10\\input.txt")
  val text = input.mkString

  val instructions = text.split("\n")
    .map(el => el.trim)



  //PART 1

  val computedResults = computeInstructions(instructions, Map((1, 1)))
  val cyclesToCheck = Array(20, 60, 100, 140, 180, 220)

  val result = cyclesToCheck.map(el => computedResults(el) * el).sum

  println("The sum of the register at the given cycles is => " + result)
  //PART 2

  computedResults
    .toVector
    .sortBy(el => el._1)
    .sliding(40, 40)
    .map(el => el.map(cycle => cycle._2).zipWithIndex)
    .map(el => el.map(cycle => createCycleString(cycle._2, cycle._1)).mkString(""))
    .foreach(println)



}

def computeInstructions(instructions: Array[String], result: Map[Int, Int]): Map[Int, Int] = {
  if (instructions.isEmpty) {
    result
  } else {
    computeInstructions(instructions.tail, compute(instructions.head, result))
  }
}

def compute(instruction: String, result: Map[Int, Int]) : Map[Int, Int] = {
  val clock = result.keys.max
  val currentVall = result(clock)
  if (instruction.startsWith("noop")) {
    result.updated(clock + 1, currentVall)
  } else if (instruction.startsWith("add")) {
    val valueToAdd = instruction.split(" ").last.toInt
    result
      .updated(clock + 1, currentVall)
      .updated(clock + 2, currentVall + valueToAdd)
  } else {
    result
  }
}

def createCycleString(cycle: Int, pos: Int): String = {
  if (cycle >= pos -1 && cycle <= pos + 1) {
    "#"
  } else {
    "."
  }
}
