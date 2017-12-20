package com.github.krivachy.advent

import org.scalatest.{Matchers, WordSpec}

class Day20Test extends WordSpec with Matchers {

  "Day 20 part 1 solution" should {
    "pass the example" in {
      Day20.solutionPart1(
        """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
          |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
          |""".stripMargin.lines.map(Day20.parseInputLine).toVector
      ) shouldBe 0
    }
  }

  "Day 20 part 2 solution" should {
    "pass the example" in {
      Day20.solutionPart2(
        """p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
          |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
          |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
          |p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>""".stripMargin.lines.map(Day20.parseInputLine).toVector
      ) shouldBe 1
    }
  }

}
