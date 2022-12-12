package be.sexysmidt
package day11

import util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

val muliplicationRegex = "old \\* \\d+".r
val additionRegex = "old \\+ \\d+".r
val squareRegex = "old \\* old".r
@main def day11(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day11\\input.txt")
  val text = input.mkString

  val monkeyData = text.split(RegexHelper.emptyLineRegex())
    .map(el => el.trim)

  val monkeys = monkeyData.map(el => monkeyParser(el)).zipWithIndex.map(el => el.swap).toMap

  val updatedMonkeys = rounds(monkeys, 10000)
  val result = updatedMonkeys.values.map(el => el.itemsHeld).toVector.sorted.reverse.take(2).map(el => el.toLong).product
  println(updatedMonkeys.values.map(el => el.itemsHeld).toVector.sorted.reverse)
  println("The product of the amount of items held by the two most active monkeys => " + result)

  //PART 1

  //PART 2




}

@tailrec
def rounds(monkeys: Map[Int, Monkey], amount: Int): Map[Int, Monkey] = {
  println("Rounds left => " + amount)
  if (amount == 0) {
    monkeys
  } else {
    rounds(performRound(monkeys, monkeys.keys.toArray.sorted), amount - 1)
  }
}

@tailrec
def performRound(monkeys: Map[Int, Monkey], keys: Array[Int]): Map[Int, Monkey] = {
  if (keys.isEmpty) {
    monkeys
  } else {
    performRound(monkeyBusiness(monkeys, keys.head), keys.tail)
  }
}

def monkeyBusiness(monkeys: Map[Int, Monkey], key: Int): Map[Int, Monkey] = {
  val monkey = monkeys(key)
  val testedItems = monkey.items.map(el => monkey.inspect(el) % monkeys.values.map(el => el.test).product).groupBy(el => el % monkey.test == 0)
  val successMonkey = monkeys(monkey.success)
  val failMonkey = monkeys(monkey.fail)
  monkeys
    .updated(key, monkey.copy(items = Array.empty[BigInt], itemsHeld = monkey.itemsHeld + monkey.items.length))
    .updated(monkey.success, successMonkey.copy(items = successMonkey.items.appendedAll(testedItems.get(true).orElse(Option(Array.empty[BigInt])).get)))
    .updated(monkey.fail, failMonkey.copy(items = failMonkey.items.appendedAll(testedItems.get(false).orElse(Option(Array.empty[BigInt])).get)))
}
def monkeyParser(monkeyText: String): Monkey = {
  val lines = monkeyText.split("\n").tail
  val items = lines(0).split(":").last.trim.split(",").map(el => BigInt(el.trim))
  val inspect = inspectionFunction(lines(1).split("=").last.trim)
  val test = BigInt(lines(2).split(" ").last.trim)
  val fail = lines(3).split(" ").last.trim.toInt
  val success = lines(4).split(" ").last.trim.toInt
  Monkey(items, inspect, test, fail, success, 0)
}

def inspectionFunction(text: String): (BigInt) => BigInt = {
  if (muliplicationRegex.matches(text)) {
    val value = BigInt(text.split(" ").last)
    (item: BigInt) => item * value //checkAndMultiplyByPrime(item, value)
  } else if (additionRegex.matches(text)) {
    val value = BigInt(text.split(" ").last.toLong)
    (item: BigInt) => item + value
  } else if (squareRegex.matches(text)) {
    (item: BigInt) => item * item
  } else {
    (item: BigInt) => item
  }
}

def checkAndMultiplyByPrime(number: BigInt, primeToCheck: BigInt): BigInt = {
  if  (number % primeToCheck == 0) {
    number
  } else {
    number * primeToCheck
  }
}

def checkAndSquare(number: BigInt): BigInt = {
  if (number > 9999999999999999L) {
    number
  } else {
    number * number
  }
}

