package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}
import Day21._

class Day21Test extends WordSpec with Matchers {

  private val simpleImage = parseFullImage {
    """..#
      |...
      |...""".stripMargin
  }

  private val simpleImage90 = parseFullImage {
    """...
      |...
      |..#""".stripMargin
  }

  private val simpleImage180 = parseFullImage {
    """...
      |...
      |#..""".stripMargin
  }

  private val simpleImage270 = parseFullImage {
    """#..
      |...
      |...""".stripMargin
  }

  "The basics functions" should {
    "work for all rotations" in {
      allRotationsFor(simpleImage) shouldBe Seq(
        simpleImage, simpleImage90, simpleImage180, simpleImage270
      )
    }
    "work for all flips" in {
      mirrorOnX(simpleImage) shouldBe simpleImage90
      mirrorOnY(simpleImage) shouldBe simpleImage270
    }
    "work for transposing a square" in {
      transposeImage(simpleImage) shouldBe simpleImage180
    }
    "work for transposing a non-square" in {
      transposeImage(parseFullImage {
        """..#..#
          |......
          |......""".stripMargin
      }) shouldBe parseFullImage {
        """...
          |...
          |#..
          |...
          |...
          |#..""".stripMargin
      }
    }
    "work for combining images in a row" in {
      combineImagesInARow(simpleImage, simpleImage) shouldBe parseFullImage {
        """..#..#
          |......
          |......""".stripMargin
      }
    }
    "work for dividing images" in {
      val in = parseFullImage {
        """.#.#
          |#..#
          |#..#
          |....""".stripMargin
      }
      divideImageInto(2)(in) shouldBe Vector(
        Vector(".#/#.", ".#/.#").map(parseFlatImage),
        Vector("#./..", ".#/..").map(parseFlatImage)
      )
    }
    "work for combining images" in {
      val in = Vector(
        Vector(".#/#.", ".#/.#").map(parseFlatImage),
        Vector("#./..", ".#/..").map(parseFlatImage)
      )
      val out = parseFullImage {
        """.#.#
          |#..#
          |#..#
          |....""".stripMargin
      }
      recombineIntoWholeImage(in) shouldBe out
    }
    "work for dividing and recombining images" in {
      val in = parseFullImage {
        """.#.#
          |#..#
          |#..#
          |....""".stripMargin
      }
      recombineIntoWholeImage(divideImageInto(2)(in)) shouldBe in
    }
  }

  "Day 21 part 1 solution" should {
    "pass the example" in {
      Day21.solution(2,
        """../.# => ##./#../...
          |.#./..#/### => #..#/..../..../#..#""".stripMargin.lines.map(Day21.parseInputLine).toMap
      ) shouldBe 12
    }
  }

}
