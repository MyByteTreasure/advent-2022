package be.sexysmidt
package day8

import util.{RegexHelper, StringHelper}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

val maxSize = 100000
val diskSpace = 70000000
val neededSpace = 30000000
@main def day8(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day8\\input.txt")
  val text = input.mkString

  val baseInput = text.split("\n")
    .map(el => el.trim)
    .filter(StringHelper.checkNotBlank)
    .map(el => el.split(""))

  //PART 1

  val treeList = baseInput
    .zipWithIndex
    .flatMap(indexedArr => indexedArr._1.zipWithIndex.map( el => Tree(el._1.toInt, false, indexedArr._2, el._2)))

  val checkedTrees = checkTreesInPlot(treeList)

  val amount = checkedTrees.count(el => el.visible)

  println("The amount of visible trees => " + amount )

  //PART 2
  val positionMap = treeList.map(el => ((el.row, el.column), el)).toMap
  val scores = checkScenicScoreTrees(positionMap)
  val highest = scores.max



  println("The hightest scenic score is => " + highest)

}

def checkTreesInPlot(treePlot: Array[Tree]): Array[Tree] = {
  val checkedRows = treePlot
    .groupBy(el => el.row)
    .values
    .map(el => el.sortBy(tree => tree.column))
    .flatMap(checkTreeLinesBothDirections)

  checkedRows
    .groupBy(el => el.column)
    .values
    .map(el => el.toVector.sortBy(tree => tree.row))
    .flatMap(el => checkTreeLinesBothDirections(el.toArray)).toArray
}
def checkTreeLinesBothDirections(trees: Array[Tree]): Array[Tree] = {
  val leftToRight = checkTreeLine(trees, -1, Array.empty)

  val rightToLeft = checkTreeLine(leftToRight.reverse, -1, Array.empty)

  rightToLeft.reverse
}

def checkTreeLine(treesToCheck: Array[Tree], currHeight: Int, checkedTrees: Array[Tree]): Array[Tree] = {
  if  (treesToCheck.isEmpty) {
    checkedTrees
  } else {
    val tree = treesToCheck.head
    val updatedTree = checkIfVisible(tree, currHeight)
    val updatedHeight = Vector(tree.height, currHeight).max
    checkTreeLine(treesToCheck.tail, updatedHeight, checkedTrees.appended(updatedTree))
  }
}

def checkIfVisible(tree: Tree, currHeight: Int): Tree = {
  if (tree.visible) {
    tree
  } else {
    tree.copy(visible = tree.height > currHeight)
  }
}

def checkScenicScoreTrees(positionMap: Map[(Int, Int), Tree]): Array[Int] = {
  val scorePerDirection = positionMap.values.map(el => {
    Array(
      checkScenicScore(el, (1, 0), positionMap, 0, 1),
      checkScenicScore(el, (0, 1), positionMap, 0, 1),
      checkScenicScore(el, (-1, 0), positionMap, 0, 1),
      checkScenicScore(el, (0, -1), positionMap, 0, 1),
    )
  })
  scorePerDirection.map(el => el.product).toArray
}

@tailrec
def checkScenicScore(tree: Tree, step: (Int, Int), positionMap: Map[(Int, Int), Tree], score: Int, increase: Int): Int = {
  val otherTree = positionMap.get((tree.row + (step._1 * increase), tree.column + (step._2 * increase)))
  println(step)
  println(score)
  if (otherTree.isEmpty) {
    score
  } else if (otherTree.get.height >= tree.height){
    score + 1
  } else {
    checkScenicScore(tree, (step._1, step._2), positionMap, score + 1, increase + 1)
  }
}

