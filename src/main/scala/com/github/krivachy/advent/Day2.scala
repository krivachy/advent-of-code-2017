package com.github.krivachy.advent

import scala.io.Source

object Day2 extends App {


  def solutionPart1(input: Seq[Seq[Int]]): Int = {
    input.foldLeft(0) {
      case (sum, nextRow) => sum + (nextRow.max - nextRow.min)
    }
  }

  private def tryDivisibilityOneWay(a: Int, b: Int): Option[Int] = {
    if (a % b == 0) Some(a / b) else None
  }

  private def tryDivisibility(a: Int, b: Int): Option[Int] = {
    tryDivisibilityOneWay(a, b)
      .orElse(tryDivisibilityOneWay(b, a))
  }

  def solutionPart2(input: Seq[Seq[Int]]): Int = {
    input.foldLeft(0) {
      case (sum, nextRow) =>
        sum + nextRow
          // take all combinations of pairs
          .combinations(2)
          // try divisibility both ways and only keep the ones where it succeeded
          .flatMap { case a :: b :: Nil => tryDivisibility(a, b) }
          .toList
          // take first one that succeeded
          .headOption
          .getOrElse(0)
    }
  }


  val problemInput = Source.fromResource("day2.txt").getLines.map(_.split('\t').map(_.toInt).toList).toList
  println(solutionPart1(problemInput))
  println(solutionPart2(problemInput))

}
