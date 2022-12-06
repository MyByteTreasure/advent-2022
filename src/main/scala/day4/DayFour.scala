package be.sexysmidt
package day4

import be.sexysmidt.util.StringHelper

import java.util.regex.Pattern
import scala.annotation.switch
import scala.io.Source

@main def calculateScore(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day4\\input.txt")
  val text = input.mkString

  val cleanedInput = text.split("\n")
    .filter(StringHelper.checkNotBlank)
    .map(el => el.trim)

  //PART 1

  val elfPairRanges = cleanedInput
    .map(el => el.split(","))
    .map(el => el.map(splitAndParseToIntTuple))
    .map(el => (el(0), el(1)))

  val fullyContainedPairs = elfPairRanges.filter(isFullyContainedBiDirectional)

  println("Total amount of fully contained pairs => " + fullyContainedPairs.length )

  //PART 2

  val overlappingPairs = elfPairRanges.filter(isOverlappingBiDirectional)

  println("Total amount of overlapping pairs => " + overlappingPairs.length)

}

def splitAndParseToIntTuple(text: String): (Int, Int) = {
  val integers = text.split("-").map(el => el.toInt)
  (integers(0), integers(1))
}

def isFullyContainedBiDirectional(integerPairTupple: ((Int, Int), (Int, Int))): Boolean = {
  isFullyContained(integerPairTupple._1, integerPairTupple._2) || isFullyContained(integerPairTupple._2, integerPairTupple._1)
}

def isFullyContained(pairOne: (Int, Int), pairTwo: (Int, Int)): Boolean = {
  pairOne._1 >= pairTwo._1 && pairOne._2 <= pairTwo._2
}

def isOverlappingBiDirectional(integerPairTupple: ((Int, Int), (Int, Int))): Boolean = {
  isOverlapping(integerPairTupple._1, integerPairTupple._2) || isOverlapping(integerPairTupple._2, integerPairTupple._1)
}

def isOverlapping(pairOne: (Int, Int), pairTwo: (Int, Int)): Boolean = {
  (pairOne._1 >= pairTwo._1 && pairOne._1 <= pairTwo._2) || (pairOne._2 >= pairTwo._1 && pairOne._2 <= pairTwo._2)
}

