package be.sexysmidt
package day2

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
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day2\\input.txt")
  val text = input.mkString

  val cleanedInput = text.split("\n")
    .filter(StringHelper.checkNotBlank)
    .map(el => el.trim)

  //PART 1
  val handShapeTuples = cleanedInput
    .map(el => el.split(" "))
    .map(el => (toElfHandShape(el(0)), toElfHandShape(el(1))))

  val score = handShapeTuples
    .map(determineScore)
    .sum
  print(score)
  
  
}


def determineScore(handShapes: (String, String)): Int = {
  getOutComeScore(handShapes) + toHandShapeScore(handShapes._2)
}

def getOutComeScore(handShapes: (String, String)): Int = {
  if (handShapes._1.equals(handShapes._2)) {
    3
  } else if (hasWon(handShapes._1, handShapes._2)) {
    6
  } else {
    0
  }
}

def hasWon(elfHandShape: String, myHandShape: String): Boolean = {
  elfHandShape.equals(rock) && myHandShape.equals(paper)
    || (elfHandShape.equals(paper) && myHandShape.equals(scissors))
    || (elfHandShape.equals(scissors) && myHandShape.equals(rock))
}

def toElfHandShape(rawString: String): String = (rawString: @switch) match {
  case "A" => rock
  case "B" => paper
  case "C" => scissors
  case _ => throw RuntimeException("Invalid coding")
}

def toMyHandShape(elfHandShape: String, myRawString: String): String =  {
  if (myRawString.equals("Y")) {
    elfHandShape
  } else if (myRawString.equals("X")) {
    map.get(elfHandShape).get._1
  } else {
    map.get(elfHandShape).get._2

  }
}

def toHandShapeScore(handShape: String): Int = (handShape: @switch) match {
  case "Rock" => 1
  case "Paper" => 2
  case "Scissors" => 3
  case _ => throw RuntimeException("Invalid handshape")
}

