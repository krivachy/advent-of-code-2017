package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day2Test extends WordSpec with Matchers {

  "The day 2 part 1 solution" should {
    "pass the example" in {
      Day2.solutionPart1(Seq(
        Seq(5, 1, 9, 5),
        Seq(7, 5, 3),
        Seq(2, 4, 6, 8)
      )) shouldBe 18
    }
  }
  "The day 2 part 2 solution" should {
    "pass the example" in {
      Day2.solutionPart2(Seq(
        Seq(5, 9, 2, 8),
        Seq(9, 4, 7, 3),
        Seq(3, 8, 6, 5)
      )) shouldBe 9
    }
  }

}
