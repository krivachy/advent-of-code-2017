package com.github.krivachy.advent

import scala.annotation.tailrec
import scala.io.Source

/**
  * --- Day 11: Hex Ed ---
  *
  * Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"
  *
  * Fortunately for her, you have plenty of experience with infinite grids.
  *
  * Unfortunately for you, it's a hex grid.
  *
  * The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:
  *
  *   \ n  /
  * nw +--+ ne
  *   /    \
  * -+      +-
  *   \    /
  * sw +--+ se
  *   / s  \
  * You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)
  *
  * For example:
  *
  * ne,ne,ne is 3 steps away.
  * ne,ne,sw,sw is 0 steps away (back where you started).
  * ne,ne,s,s is 2 steps away (se,se).
  * se,sw,se,sw,sw is 3 steps away (s,s,sw).
  *
  * --- Part Two ---
  *
  * How many steps away is the furthest he ever got from his starting position?
  */
object Day11 extends App {

  private lazy val NegatingSteps = Seq("n" -> "s", "nw" -> "se", "sw" -> "ne")
  private lazy val EquivalentSteps = List(
    ("n", "se") -> "ne",
    ("ne", "s") -> "se",
    ("se", "sw") -> "s",
    ("s", "nw") -> "sw",
    ("sw", "n") -> "nw",
    ("nw", "ne") -> "n"
  )


  private def negatedStepRemoval(initialStepCounts: Map[String, Int]) = {
    NegatingSteps.foldLeft(initialStepCounts) {
      case (stepCounts, (directionA, directionB)) =>
        val countA = stepCounts.getOrElse(directionA, 0)
        val countB = stepCounts.getOrElse(directionB, 0)
        val min = Math.min(countA, countB)
        stepCounts.updated(directionA, countA - min).updated(directionB, countB - min)
    }
  }

  @tailrec
  private def recurseEquivalentStepRemoval(stepCounts: Map[String, Int], nextEquivalentSteps: List[((String, String), String)], lastRemovalCount: Int): Map[String, Int] = {
    if (lastRemovalCount >= 6) stepCounts
    else {
      nextEquivalentSteps match {
        case ((stepA, stepB), singleStep) :: rest =>
          val countA = stepCounts.getOrElse(stepA, 0)
          val countB = stepCounts.getOrElse(stepB, 0)
          val stepsToRemove = Math.min(countA, countB)
          val updatedSteps = stepCounts
            .updated(stepA, countA - stepsToRemove)
            .updated(stepB, countB - stepsToRemove)
            .updated(singleStep, stepCounts.getOrElse(singleStep, 0) + stepsToRemove)
          recurseEquivalentStepRemoval(
            updatedSteps,
            rest :+ ((stepA, stepB), singleStep),
            if (stepsToRemove == 0) lastRemovalCount + 1 else 0
          )
        case _ =>
          recurseEquivalentStepRemoval(stepCounts, nextEquivalentSteps, lastRemovalCount + 1)
      }
    }
  }

  def removePointlessSteps(stepCounts: Map[String, Int]): Map[String, Int] = {
    val negatedStepCounts = negatedStepRemoval(stepCounts)
    recurseEquivalentStepRemoval(negatedStepCounts, EquivalentSteps, 0)
  }

  def solutionPart1(steps: Seq[String]): Int = {
    val groupedStepCounts = steps.groupBy(identity).mapValues(_.length)
    removePointlessSteps(groupedStepCounts).values.sum
  }

  def solutionPart2(inputSteps: Seq[String]): Int = {
    val initialMax = 0
    val initialStepCounts = Map.empty[String, Int]
    val (finalMax, _) = inputSteps.foldLeft((initialMax, initialStepCounts)) {
      case ((max, stepCounts), nextStep) =>
        val updatedStepCounts = removePointlessSteps(
          stepCounts.updated(nextStep, stepCounts.getOrElse(nextStep, 0) + 1)
        )
        val maxSteps = updatedStepCounts.values.sum
        val newMax = Math.max(max, maxSteps)
        (newMax, updatedStepCounts)
    }
    finalMax
  }


  val problemInput = Source.fromResource("day11.txt").getLines().toSeq.head.split(',').toSeq
  println(solutionPart1(problemInput))
  println(solutionPart2(problemInput))
}
