package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day10Test extends WordSpec with Matchers {

  "The Day 10 part 1 solution" should {
    "pass the example" in {
      Day10.solutionPart1(5, Seq(3, 4, 1, 5)) shouldBe 12
    }
  }

  "The Day 10 part 2 solution" should {
    "pass the first example" in {
      Day10.solutionPart2("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    }
    "pass the second example" in {
      Day10.solutionPart2("AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    }
    "pass the third example" in {
      Day10.solutionPart2("1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    }
    "pass the fourth example" in {
      Day10.solutionPart2("1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
    }
  }

}
