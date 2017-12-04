package com.github.krivachy.advent

import scala.io.Source

object Day4 extends App {

  def solutionPart1(passphrases: Seq[Seq[String]]): Int = {
    passphrases.map { passphrase =>
      passphrase.distinct.size == passphrase.size
    }.count(identity)
  }

  def solutionPart2(passphrases: Seq[Seq[String]]): Int = {
    passphrases.map { passphrase =>
      val (isValid, _) = passphrase.foldLeft((true, Set.empty[Map[Char, Int]])) {
        case ((valid, anagramMap), word) =>
          val letterOccurrences = word.toCharArray.groupBy(identity).mapValues(_.length)
          if (anagramMap.contains(letterOccurrences)) (false, anagramMap + letterOccurrences)
          else (valid, anagramMap + letterOccurrences)
      }
      isValid
    }.count(identity)
  }


  val problemInput = Source.fromResource("day4.txt").getLines.map(_.split(" ").toList).toList
  println(solutionPart1(problemInput))
  println(solutionPart2(problemInput))
}
