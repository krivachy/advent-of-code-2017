package com.github.krivachy.advent

import scala.io.Source

object Day1 extends App {

  def solutionPart1(inputList: List[Char]) = {
    val wrapAroundInput = inputList ++ inputList.headOption

    wrapAroundInput
      .sliding(2)
      .foldLeft(0) {
        case (sum, a :: b :: Nil) =>
          if (a == b) sum + a.asDigit
          else sum
        case (sum, _) =>
          // just in case any other pattern shows up, which should be impossible due to sliding(2).
          // Not fully typesafe, ideally i'd like a sliding2 method that would return a tuple to
          // safely pattern match on.
          sum
      }
  }

  def solutionPart2(input: List[Char]) = {
    val half = input.size / 2
    val rotatedInput = input.takeRight(half) ++ input.take(half)
    input.zip(rotatedInput).foldLeft(0){
      case (sum, (a, b)) => if (a == b) sum + a.asDigit else sum
    }
  }

  val inputFile = Source.fromResource("day1.txt").getLines.mkString("\n").trim.toList
  println(solutionPart1(inputFile))
  println(solutionPart2(inputFile))
}
