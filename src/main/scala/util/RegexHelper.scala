package be.sexysmidt
package util

object RegexHelper {

  def emptyLineRegex(): String = {
    "(?m)^\\s*$"
  }

  def integerRegex(): String = {
    "(\\d+)"
  }
}
