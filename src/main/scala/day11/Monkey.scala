package be.sexysmidt
package day11

case class Monkey(items: Array[BigInt], inspect: (BigInt) => BigInt, test: BigInt, success: Int, fail: Int, itemsHeld: Int)
