package com.github.krivachy.advent

import scala.io.Source
import Function.tupled

/**
  * --- Day 21: Fractal Art ---
  * You find a program trying to generate some art. It uses a strange process that involves repeatedly enhancing the detail of an image through a set of rules.
  *
  * The image consists of a two-dimensional square grid of pixels that are either on (#) or off (.). The program always begins with this pattern:
  *
  * .#.
  * ..#
  * ###
  * Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have a size of 3.
  *
  * Then, the program repeats the following process:
  *
  * If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and convert each 2x2 square into a 3x3 square by following the corresponding enhancement rule.
  * Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares, and convert each 3x3 square into a 4x4 square by following the corresponding enhancement rule.
  * Because each square of pixels is replaced by a larger one, the image gains pixels and so its size increases.
  *
  * The artist's book of enhancement rules is nearby (your puzzle input); however, it seems to be missing rules. The artist explains that sometimes, one must rotate or flip the input pattern to find a match. (Never rotate or flip the output pattern, though.) Each pattern is written concisely: rows are listed as single units, ordered top-down, and separated by slashes. For example, the following rules correspond to the adjacent patterns:
  *
  * ../.#  =  ..
  *           .#
  *
  *                 .#.
  * .#./..#/###  =  ..#
  *                 ###
  *
  *                         #..#
  * #..#/..../#..#/.##.  =  ....
  *                         #..#
  *                         .##.
  * When searching for a rule to use, rotate and flip the pattern as necessary. For example, all of the following patterns match the same rule:
  *
  * .#.   .#.   #..   ###
  * ..#   #..   #.#   ..#
  * ###   ###   ##.   .#.
  * Suppose the book contained the following two rules:
  *
  * ../.# => ##./#../...
  * .#./..#/### => #..#/..../..../#..#
  * As before, the program begins with this pattern:
  *
  * .#.
  * ..#
  * ###
  * The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly into a single square; the square matches the second rule, which produces:
  *
  * #..#
  * ....
  * ....
  * #..#
  * The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides evenly into four squares:
  *
  * #.|.#
  * ..|..
  * --+--
  * ..|..
  * #.|.#
  * Each of these squares matches the same rule (../.# => ##./#../...), three of which require some flipping and rotation to line up with the rule. The output for the rule is the same in all four cases:
  *
  * ##.|##.
  * #..|#..
  * ...|...
  * ---+---
  * ##.|##.
  * #..|#..
  * ...|...
  * Finally, the squares are joined into a new grid:
  *
  * ##.##.
  * #..#..
  * ......
  * ##.##.
  * #..#..
  * ......
  * Thus, after 2 iterations, the grid contains 12 pixels that are on.
  *
  * How many pixels stay on after 5 iterations?
  *
  * --- Part Two ---
  * How many pixels stay on after 18 iterations?
  */
object Day21 {

  type Image = Vector[Vector[Char]]
  private val initialImage: Image = parseFullImage {
    """.#.
      |..#
      |###""".stripMargin
  }

  def solution(iterations: Int, imageReplacementLookup: Map[Image, Image]): Int = {
    val iterator = Iterator.iterate(initialImage)(runIteration(imageReplacementLookup))
    val finalImage = iterator.slice(1, iterations + 1).toList.last
    finalImage.map(_.count(_ == '#')).sum
  }

  /*
    Broken down functions
   */
  private[advent] def runIteration(imageReplacementLookup: Map[Image, Image])(image: Image): Image = {
    val subSquareSizes = image.length match {
      case divisibleBy2 if divisibleBy2 % 2 == 0 => 2
      case divisibleBy3 if divisibleBy3 % 3 == 0 => 3
      case _ =>
        throw new IllegalStateException(s"Unexpected image size ${image.length}:\n${image.map(_.mkString).mkString("\n")}")
    }
    val brokenUpImages = divideImageInto(subSquareSizes)(image)
    val replacedBrokenUpImages = brokenUpImages.map(_.map(findAndReplaceImage(imageReplacementLookup)))
    recombineIntoWholeImage(replacedBrokenUpImages)
  }

  private[advent] def findAndReplaceImage(lookup: Map[Image, Image])(image: Image): Image = {
    allRotationsAndFlipsFor(image).flatMap(lookup.get).head
  }

  private[advent] def divideImageInto(sizeOfSubSquares: Int)(image: Image): Vector[Vector[Image]] = {
    image.grouped(sizeOfSubSquares).map {
      firstNRows => transposeImage(firstNRows).grouped(sizeOfSubSquares).toVector.map(transposeImage)
    }.toVector
  }

  private[advent] def combineImagesInARow(imageA: Image, imageB: Image): Image = {
    imageA.zip(imageB).map(tupled(_ ++ _))
  }

  private[advent] def recombineIntoWholeImage(brokenUpImages: Vector[Vector[Image]]): Image = {
    brokenUpImages.flatMap(_.reduce(combineImagesInARow))
  }

  // Basics:
  private[advent] def transposeImage(image: Image): Image = image.head.indices.map(i => image.map(_(i))).toVector
  private[advent] def rotateImage90(image: Image): Image = mirrorOnY(transposeImage(image))
  private[advent] def mirrorOnX(image: Image): Image = image.reverse
  private[advent] def mirrorOnY(image: Image): Image = image.map(_.reverse)

  // Combinations:
  private[advent] def allRotationsFor(image: Image): Vector[Image] = {
    val rotation90 = rotateImage90(image)
    val rotation180 = rotateImage90(rotation90)
    val rotation270 = rotateImage90(rotation180)
    Vector(image, rotation90, rotation180, rotation270)
  }

  private[advent] def allFlipsFor(image: Image): Vector[Image] = {
    Vector(image, mirrorOnX(image), mirrorOnY(image))
  }

  private[advent] def allRotationsAndFlipsFor(image: Image): Vector[Image] = {
    allRotationsFor(image).flatMap(allFlipsFor).distinct
  }

  /*
    INPUT PARSING
   */
  private[advent] def parseFullImage(imageStr: String): Image = {
    imageStr.lines.map(_.toCharArray.toVector).toVector
  }

  private[advent] def parseFlatImage(flatImage: String): Image = {
    flatImage.split("/").map(_.toCharArray.toVector).toVector
  }

  def parseInputLine(line: String): (Image, Image) = {
    line.split(" => ").toList match {
      case from :: to :: Nil => (parseFlatImage(from), parseFlatImage(to))
      case _ => throw new IllegalArgumentException("Unrecognized line: " + line)
    }
  }

  /*
    MAIN
   */

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day21.txt").getLines.map(parseInputLine).toMap
    println(solution(5, input))
    println(solution(18, input))
  }
}
