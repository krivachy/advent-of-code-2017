package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day19Test extends WordSpec with Matchers {

  // margins needed so editors don't automatically nuke extra spaces
  private val input = """     |          #
                        #     |  +--+    #
                        #     A  |  C    #
                        # F---|----E|--+ #
                        #     |  |  |  D #
                        #     +B-+  +--+ #
                        #                #"""
    .stripMargin('#') // left margin
    .lines
    .map(_.stripSuffix("#").toVector) // right margin
    .toVector

  "Day 19 part 1 solution" should {
    "pass the example" in {
      Day19.solutionPart1(input) shouldBe "ABCDEF"
    }
  }
  "Day 19 part 2 solution" should {
    "pass the example" in {
      Day19.solutionPart2(input) shouldBe 38
    }
  }

}
