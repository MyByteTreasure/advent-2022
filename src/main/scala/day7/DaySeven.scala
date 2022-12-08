package be.sexysmidt
package day7

import be.sexysmidt.util.{RegexHelper, StringHelper}
import be.sexysmidt.day7.{FileSystem, Directory}

import java.util.regex.Pattern
import scala.annotation.{switch, tailrec}
import scala.io.Source

val maxSize = 100000
val diskSpace = 70000000
val neededSpace = 30000000
@main def day7(): Unit = {
  val input = Source.fromFile("C:\\Users\\smidtma\\IdeaProjects\\personal\\advent\\2022\\advent-2022\\src\\main\\scala\\day7\\input.txt")
  val text = input.mkString

  val baseInput = text.split("\n")
    .filter(StringHelper.checkNotBlank)
    .map(el => el.trim)

  //PART 1
  val instructionsWithoutLs = baseInput.filter(el => !el.contains("$ ls"))
  val initialFileSystem = FileSystem(Map(("/", Directory(name= "/", parentName = null, size = 0))), "/")
  val fileSystem = createDirectoryMap(initialFileSystem, instructionsWithoutLs.tail)

  val sumOfSizesSmallerThen = fileSystem.directoryMap.values
    .filter(el => el.size < maxSize )
    .map(el => el.size)
    .sum
  println("The sum of all directories with size less than 100000 => " + sumOfSizesSmallerThen )

  //PART 2

  val totalSize = fileSystem.directoryMap.get("/").get.size
  val diskSpaceToFreeUp = neededSpace - (diskSpace - totalSize)
  val directoryToDelete = fileSystem.directoryMap.values
    .filter(el => el.size > diskSpaceToFreeUp)
    .toVector
    .minBy(el => el.size)


  println("The smalles directory to delete has size => " + directoryToDelete.size)

}

def createDirectoryMap(fileSystem: FileSystem, instructions: Array[String]): FileSystem = {
  if (instructions.isEmpty) {
    fileSystem
  } else {
    val updatedFileSystem = performInstruction(fileSystem, instructions.head)
    createDirectoryMap(updatedFileSystem, instructions.tail)
  }
}

def performInstruction(fileSystem: FileSystem, instruction: String): FileSystem = {
  if (instruction.startsWith("dir")) {
    val directory = addDirectory(fileSystem.directoryMap, fileSystem.currentDirName, instruction.split(" ").last)
    fileSystem.copy(directory)
  } else if (instruction.startsWith("$ cd")) {
    updateCurrentDirectory(fileSystem, instruction.split(" ").last)
  } else {

    //case if file size
    val size = instruction.split(" ").head.toInt
    fileSystem.copy(addSizeToDirectory(fileSystem.directoryMap, fileSystem.currentDirName, size))
  }
}

def addDirectory(directoryMap: Map[String, Directory], parentName: String, name: String): Map[String, Directory] = {
  val path = parentName + name + "/"
  directoryMap.updated(path, Directory(parentName, path, 0))
}

@tailrec
def addSizeToDirectory(directoryMap: Map[String, Directory], name: String, size: Int): Map[String, Directory] = {
  if (name == null) {
    directoryMap
  } else {
    val dir = directoryMap.get(name).get
    val updatedMap = directoryMap.updated(dir.name, dir.copy(size = dir.size + size))
    addSizeToDirectory(updatedMap, dir.parentName, size)
  }
}

def updateCurrentDirectory(fileSystem: FileSystem, command: String): FileSystem = {
  if (command.equals("..")) {
    val newDirectory = fileSystem.directoryMap.get(fileSystem.currentDirName).get
    fileSystem.copy(currentDirName = newDirectory.parentName)
  } else if (command.equals("/")) {
    fileSystem.copy(currentDirName = "/")
  } else {
    fileSystem.copy(currentDirName = fileSystem.currentDirName + command + "/")
  }
}
