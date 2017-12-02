package com.github.krivachy.advent

import scala.io.Source

object Day2 extends App {


  def solution(input: Seq[Seq[Int]]): Int = {
    input.foldLeft(0) {
      case (sum, nextRow) => sum + (nextRow.max - nextRow.min)
    }
  }


  val problemInput = Source.fromResource("day2.txt").getLines.map(_.split('\t').map(_.toInt).toList).toList
  println(solution(problemInput))

}
