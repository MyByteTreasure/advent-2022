package be.sexysmidt
package day6

import util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

val startMarkerLength = 4
val messageMarkerLength = 14

@main def calculateScore(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day6\\input.txt")
  val text = input.mkString.trim

  //PART 1


  val startSignalIndex = amountOfCharsBeforeUniqueSequence(text, startMarkerLength)

  println("The startsignal index is => "  + startSignalIndex)

  //PART 2

  val messageIndex = amountOfCharsBeforeUniqueSequence(text, messageMarkerLength)


  println("The messageSignal index is => " + messageIndex)

}

def amountOfCharsBeforeUniqueSequence(text: String, sequenceLength: Int): Int = {
  val windows = text.sliding(sequenceLength)

  val windowSetsWithIndex = windows
    .map(el => el.split(""))
    .map(el => el.toSet.toVector)
    .zipWithIndex

  val startSignal = windowSetsWithIndex.find(el => el._1.length == sequenceLength)

  startSignal.get._2 + sequenceLength
}

