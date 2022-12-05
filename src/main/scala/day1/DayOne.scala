package be.sexysmidt
package day1

import be.sexysmidt.util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.io.Source

@main def calculateMaxFromFile(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day1\\input.txt")
  val text = input.mkString

  val result = text.split(RegexHelper.emptyLineRegex())
    .filter(StringHelper.checkNotBlank)
    .map(stringGroupToInt)
    .map(el => el.sum)
    .sorted
    .reverse
    .take(3)
    .sum


    print(result)
}

def stringGroupToInt(numbers: String): Array[Int] = {
  numbers.split("\n").filter(StringHelper.checkNotBlank).map(el => el.trim).map(el => el.toInt)
}

