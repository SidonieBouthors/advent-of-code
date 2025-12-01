package aoc2025.day01

import scala.io.Source

enum Direction:
  case L, R
import Direction.*

case class Rotation(dir: Direction, dist: Int):
  lazy val delta: Int =
    dir match
      case L => -dist
      case R => dist
object Rotation:
  def fromString(s: String): Rotation =
    s match
      case s if s.startsWith("L") => Rotation(L, s.tail.toInt)
      case s if s.startsWith("R") => Rotation(R, s.tail.toInt)
      case _ => throw IllegalArgumentException(s"Invalid rotation string: $s")

case class Position(value: Int):
  require(value >= 0 && value < 100)
  infix def +(rot: Rotation) = Position((value + rot.delta % 100 + 100) % 100)
  def isZero: Boolean = value == 0
object Position:
  val Start = Position(50)

@main def day01(): Unit =
  val input = Source.fromFile("../input/day01.txt").mkString
  val rotations = input.linesIterator.toList.map(Rotation.fromString)
  val answer1 = part1(rotations)
  println(s"The solution to Part 1 is $answer1")
  val answer2 = part2(rotations)
  println(s"The solution to Part 2 is $answer2")

def part1(rotations: List[Rotation]): Int =
  rotations.scanLeft(Position.Start)(_ + _).count(_.isZero)

def countZeros(pos: Position, rot: Rotation): Int =
  (1 to rot.dist).count(i => (pos + Rotation(rot.dir, i)).isZero)

def part2(rotations: List[Rotation]): Int =
  rotations
    .foldLeft((Position.Start, 0)) { case ((pos, crosses), rot) =>
      (pos + rot, crosses + countZeros(pos, rot))
    }
    ._2
