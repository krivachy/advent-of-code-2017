package com.github.krivachy.advent

import scala.annotation.tailrec
import scala.io.Source

/**
  * --- Day 19: A Series of Tubes ---
  * Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input), but it's confused about where to go.
  *
  * Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take, starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it reaches the end (located somewhere within the diagram) and stop there.
  *
  * Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't change its direction, but it can use them to keep track of where it's been. For example:
  *
  *      |
  *      |  +--+
  *      A  |  C
  *  F---|----E|--+
  *      |  |  |  D
  *      +B-+  +--+
  *
  * Given this diagram, the packet needs to take the following path:
  *
  * Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the first +.
  * Travel right, up, and right, passing through B in the process.
  * Continue down (collecting C), right, and up (collecting D).
  * Finally, go all the way left through E and stopping at F.
  * Following the path to the end, the letters it sees on its path are ABCDEF.
  *
  * The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line wrapping.)
  *
  * --- Part Two ---
  * The packet is curious how many steps it needs to go.
  *
  * For example, using the same routing diagram from the example above...
  *
  *      |
  *      |  +--+
  *      A  |  C
  *  F---|--|-E---+
  *      |  |  |  D
  *      +B-+  +--+
  *
  * ...the packet would go:
  *
  * 6 steps down (including the first line at the top of the diagram).
  * 3 steps right.
  * 4 steps up.
  * 3 steps right.
  * 4 steps down.
  * 3 steps right.
  * 2 steps up.
  * 13 steps left (including the F it stops on).
  * This would result in a total of 38 steps.
  *
  * How many steps does the packet need to go?
  */
object Day19 {
  sealed abstract class Direction(val x: Int, val y: Int)
  object Direction {
    def rightAngleTurnsFor(direction: Direction): Seq[Direction] = direction match {
      case Up | Down => Seq(Left, Right)
      case Left | Right => Seq(Up, Down)
    }
  }
  final case object Up extends Direction(-1, 0)
  final case object Down extends Direction(1, 0)
  final case object Left extends Direction(0, -1)
  final case object Right extends Direction(0, 1)

  final case class Position(x: Int, y: Int) {
    def move(direction: Direction): Position = Position(x + direction.x, y + direction.y)
  }

  private def lookupInGrid(grid: Vector[Vector[Char]])(position: Position): Char = {
    if (position.x < 0 || position.x >= grid.length) ' '
    else if (position.y < 0 || position.y >= grid.head.length) ' '
    else grid(position.x)(position.y)
  }

  private def stepThroughNetwork(grid: Vector[Vector[Char]]): (String, Int) = {
    val charAt = lookupInGrid(grid) _
    @tailrec
    def step(encounteredChars: String, numberOfSteps: Int, position: Position, headingInDir: Direction): (String, Int) = {
      // println(s"At $position moving $headingInDir: '${charAt(position)}' => '${charAt(position.move(headingInDir))}'")
      charAt(position) match {
        // Exit of recursion
        case ' ' => (encounteredChars, numberOfSteps)
        case c if c.isLetter => step(encounteredChars + c, numberOfSteps + 1, position.move(headingInDir), headingInDir)
        // We need to turn in the single right angle direction
        case '+' =>
          val possibleNextDirs = Direction.rightAngleTurnsFor(headingInDir).filter(dir => charAt(position.move(dir)) != ' ')
          possibleNextDirs.toList match {
            case Nil => // Possible exit in case the whole puzzle ends with '+'
              (encounteredChars, numberOfSteps)
            case turnedDirection :: Nil => // A single direction we're turning towards
              step(encounteredChars, numberOfSteps + 1, position.move(turnedDirection), turnedDirection)
            case _ => // There are multiple directions, this case isn't supported
              throw new IllegalStateException(s"Found a position where it's possible to turn in multiple directions: $position")
          }
        // Move forward in all other cases, we ignore processing of | and - as they're insignificant
        case _ => step(encounteredChars, numberOfSteps + 1, position.move(headingInDir), headingInDir)
      }
    }
    // x coord: the row starting from the top row
    // y coord: the column starting from the left column
    val startingPosition = Position(x = 0, y = grid.head.indexOf('|'))
    step("", 0, startingPosition, Down)
  }

  def solutionPart1(grid: Vector[Vector[Char]]): String = stepThroughNetwork(grid)._1
  def solutionPart2(grid: Vector[Vector[Char]]): Int = stepThroughNetwork(grid)._2

  def main(args: Array[String]): Unit = {
    val input: Vector[Vector[Char]] = Source.fromResource("day19.txt").getLines.map(_.toVector).toVector
    val maxLength = input.map(_.length).max
    assert(input.forall(_.length == maxLength), "not all lines have equal length")
    println(solutionPart1(input))
    println(solutionPart2(input))
  }
}
