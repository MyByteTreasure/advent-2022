package be.sexysmidt
package day3

import be.sexysmidt.util.StringHelper

import java.util.regex.Pattern
import scala.annotation.switch
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
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day3\\input.txt")
  val text = input.mkString

  val cleanedInput = text.split("\n")
    .filter(StringHelper.checkNotBlank)
    .map(el => el.trim)

  //PART 1
  val itemPerCompartment =
    cleanedInput.map(el => (el.substring(0, el.length / 2), el.substring(el.length / 2, el.length)))

  val commonItemList = itemPerCompartment.map(el => toCommonType(el._1, el._2))

  val prioritySum = commonItemList.map(charToPriority).sum

  println("Total priority of backpacks => " + prioritySum)

  //PART 2
  val windowedByThree = cleanedInput.sliding(3, 3)

  val groupBadge = windowedByThree.map(findGroupBadge)

  val groupPrioritySum = groupBadge.map(charToPriority).sum

  println("Total priority of all elf groups => " + groupPrioritySum)

}

def toCommonType(left: String, right: String): Char = {
  left.trim.split("").filter(right.contains).head.toCharArray.head
}

def findGroupBadge(backPackStrings: Array[String]): Char = {
  backPackStrings(0).trim.split("")
    .filter(item => backPackStrings(1).contains(item) && backPackStrings(2).contains(item))
    .head.toCharArray.head
}

def charToPriority(char: Char): Int = {
  char.getNumericValue - 9 + isUpperPriority(char)
}

def isUpperPriority(char: Char): Int = {
  if (char.isUpper) {
    26
  } else {
    0
  }
}

