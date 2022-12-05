package be.sexysmidt
package util

object StringHelper {
  def checkNotBlank(text: String): Boolean = {
    !text.isBlank
  }
}
