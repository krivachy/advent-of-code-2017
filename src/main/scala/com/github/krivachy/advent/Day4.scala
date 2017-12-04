package com.github.krivachy.advent

import scala.io.Source

/**
  * --- Day 4: High-Entropy Passphrases ---
  *
  * A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password. A passphrase consists of a series of words (lowercase letters) separated by spaces.
  *
  * To ensure security, a valid passphrase must contain no duplicate words.
  *
  * For example:
  *
  * aa bb cc dd ee is valid.
  * aa bb cc dd aa is not valid - the word aa appears more than once.
  * aa bb cc dd aaa is valid - aa and aaa count as different words.
  * The system's full passphrase list is available as your puzzle input. How many passphrases are valid?
  *
  * --- Part Two ---
  *
  * For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
  *
  * For example:
  *
  * abcde fghij is a valid passphrase.
  * abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
  * a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
  * iiii oiii ooii oooi oooo is valid.
  * oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
  * Under this new system policy, how many passphrases are valid?
  */
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
