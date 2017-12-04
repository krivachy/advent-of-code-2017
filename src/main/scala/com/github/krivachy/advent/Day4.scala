package com.github.krivachy.advent

import scala.io.Source

object Day4 extends App {

  def solutionPart1(passphrases: Seq[Seq[String]]): Int = {
    passphrases.map { passphrase =>
      passphrase.distinct.size == passphrase.size
    }.count(identity)
  }


  val problemInput = Source.fromResource("day4.txt").getLines.map(_.split(" ").toList).toList
  println(solutionPart1(problemInput))
}
