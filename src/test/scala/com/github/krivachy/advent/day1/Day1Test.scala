package com.github.krivachy.advent.day1

import org.scalatest.{FlatSpec, Matchers}

class Day1Test extends FlatSpec with Matchers {

  "The day 1 solution" should "pass the first example" in {
    Day1.solution("1122") shouldBe 3
  }

  it should "pass the second example" in {
    Day1.solution("1111") shouldBe 4
  }

  it should "pass the third example" in {
    Day1.solution("1234") shouldBe 0
  }

  it should "pass the fourth example" in {
    Day1.solution("91212129") shouldBe 9
  }
}
