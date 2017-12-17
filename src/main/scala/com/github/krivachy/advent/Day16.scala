package com.github.krivachy.advent

import scala.io.Source

/**
  * --- Day 16: Permutation Promenade ---
  * You come upon a very unusual sight; a group of programs here appear to be dancing.
  *
  * There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b stands in position 1, and so on until p, which stands in position 15.
  *
  * The programs' dance consists of a sequence of dance moves:
  *
  * Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
  * Exchange, written xA/B, makes the programs at positions A and B swap places.
  * Partner, written pA/B, makes the programs named A and B swap places.
  * For example, with only five programs standing in a line (abcde), they could do the following dance:
  *
  * s1, a spin of size 1: eabcd.
  * x3/4, swapping the last two programs: eabdc.
  * pe/b, swapping programs e and b: baedc.
  * After finishing their dance, the programs end up in order baedc.
  *
  * You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs standing after their dance?
  *
  * Your puzzle answer was glnacbhedpfjkiom.
  *
  * --- Part Two ---
  * Now that you're starting to get a feel for the dance moves, you turn your attention to the dance as a whole.
  *
  * Keeping the positions they ended up in from their previous dance, the programs perform it again and again: including the first dance, a total of one billion (1000000000) times.
  *
  * In the example above, their second dance would begin with the order baedc, and use the same dance moves:
  *
  * s1, a spin of size 1: cbaed.
  * x3/4, swapping the last two programs: cbade.
  * pe/b, swapping programs e and b: ceadb.
  * In what order are the programs standing after their billion dances?
  */
object Day16 {

  sealed trait DanceMove
  final case class Spin(n: Int) extends DanceMove
  final case class Exchange(positionA: Int, positionB: Int) extends DanceMove
  final case class Partner(labelA: String, labelB: String) extends DanceMove

  private val SpinRegex = "s(\\d+)".r
  private val ExchangeRegex = "x(\\d+)\\/(\\d+)".r
  private val PartnerRegex = "p(\\w)\\/(\\w)".r

  private def exchange(state: Vector[String], positionA: Int, positionB: Int): Vector[String] = {
    state
      .updated(positionA, state(positionB))
      .updated(positionB, state(positionA))
  }

  private def runMoves(initialState: Vector[String], moves: Seq[DanceMove]): Vector[String] = {
    moves.foldLeft(initialState) {
      case (previousState, Spin(count)) =>
        previousState.takeRight(count) ++ previousState.dropRight(count)
      case (previousState, Exchange(positionA, positionB)) =>
        exchange(previousState, positionA, positionB)
      case (previousState, Partner(labelA, labelB)) =>
        val positionA = previousState.indexOf(labelA)
        val positionB = previousState.indexOf(labelB)
        exchange(previousState, positionA, positionB)
    }
  }

  def solutionPart1(initialState: Vector[String], moves: Seq[DanceMove]): String = {
    runMoves(initialState, moves).mkString
  }

  def solutionPart2(initialState: Vector[String], iterations: Int, moves: Seq[DanceMove]): String = {
    def memoizedRecursion(state: Vector[String], memory: Seq[Vector[String]], count: Int): String = {
      if (count == iterations) state.mkString
      else {
        if (memory.contains(state)) {
          memory(iterations % memory.size).mkString
        } else {
          val nextState = runMoves(state, moves)
          memoizedRecursion(
            nextState,
            memory :+ state,
            count + 1
          )
        }
      }
    }
    memoizedRecursion(initialState, Seq.empty, 0)
  }


  def parseDanceMove(s: String): DanceMove = {
    s match {
      case SpinRegex(count) => Spin(count.toInt)
      case ExchangeRegex(positionA, positionB) => Exchange(positionA.toInt, positionB.toInt)
      case PartnerRegex(labelA, labelB) => Partner(labelA, labelB)
      case _ => throw new IllegalArgumentException("Unknown dance move: " + s)
    }
  }

  def parseDanceMoves(s: String): Seq[DanceMove] = s.trim.split(",").map(parseDanceMove).toSeq

  def main(args: Array[String]): Unit = {
    val problemInput = parseDanceMoves(Source.fromResource("day16.txt").getLines.toSeq.head)
    val initialProgramState = "abcdefghijklmnop".toCharArray.map(_.toString).toVector
    println(solutionPart1(initialProgramState, problemInput))
    println(solutionPart2(initialProgramState, 1000000000, problemInput))
  }
}
