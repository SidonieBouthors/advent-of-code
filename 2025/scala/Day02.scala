package aoc2025.day02

import scala.io.Source

def isRepeatingSubstring(s: String, reps: Int): Boolean =
  val len = s.size
  len % reps == 0 && s.grouped(len / reps).distinct.size == 1

def invalidId2(id: BigInt): Boolean =
  isRepeatingSubstring(id.toString, 2)

def invalidId(id: BigInt): Boolean =
  val s = id.toString
  (2 to s.size).exists(i => isRepeatingSubstring(s, i))

case class Range(start: BigInt, end: BigInt):
  require(start <= end, s"Invalid range: $start to $end")
  def invalidIds2: Seq[BigInt] = (start to end).filter(invalidId2)
  def invalidIds: Seq[BigInt] = (start to end).filter(invalidId)
object Range:
  def fromString(s: String): Range =
    s.split("-") match
      case Array(a, b) => Range(BigInt(a), BigInt(b))
      case _ => throw IllegalArgumentException(s"Invalid range string: $s")

@main def day02(): Unit =
  val input = Source.fromFile("../input/day02.txt").mkString
  val ranges = input.split(",").toList.map(Range.fromString)
  val answer1 = part1(ranges)
  println(s"The solution to Part 1 is $answer1")
  val answer2 = part2(ranges)
  println(s"The solution to Part 2 is $answer2")

def part1(ranges: List[Range]): BigInt =
  ranges.flatMap(_.invalidIds2).sum

def part2(ranges: List[Range]): BigInt =
  ranges.flatMap(_.invalidIds).sum
