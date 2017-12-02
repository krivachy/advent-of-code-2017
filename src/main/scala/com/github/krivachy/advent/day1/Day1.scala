package com.github.krivachy.advent.day1

import scala.io.Source

object Day1 extends App {

  def solution(input: String) = {
    val inputList = input.toList
    val wrapAroundInput = inputList ++ inputList.headOption

    wrapAroundInput
      .sliding(2)
      .foldLeft(0) {
        case (sum, a :: b :: Nil) =>
          if (a == b) sum + a.asDigit
          else sum
        case (sum, _) =>
          // just in case any other pattern shows up, which should be impossible due to sliding(2)
          sum
      }
  }

  val inputFile = Source.fromResource("day1/input.txt").getLines.mkString("\n").trim
  println(solution(inputFile))
}
