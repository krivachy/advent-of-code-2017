package com.github.krivachy.advent

import org.scalatest.{FlatSpec, Matchers}

class Day2Test extends FlatSpec with Matchers {

  "The day 2 solution" should "pass the example" in {
    Day2.solution(Seq(
      Seq(5, 1, 9, 5),
      Seq(7, 5, 3),
      Seq(2, 4, 6, 8)
    )) shouldBe 18
  }

}
