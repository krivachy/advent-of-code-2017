package com.github.krivachy.advent

import cats._
import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * --- Day 7: Recursive Circus ---
  *
  * Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves into a bit of trouble. A recursive algorithm has gotten out of hand, and now they're balanced precariously in a large tower.
  *
  * One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the disc below them balanced but with no disc of their own.
  *
  * You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.
  *
  * For example, if your list is the following:
  *
  * pbga (66)
  * xhth (57)
  * ebii (61)
  * havc (66)
  * ktlj (57)
  * fwft (72) -> ktlj, cntj, xhth
  * qoyq (66)
  * padx (45) -> pbga, havc, qoyq
  * tknk (41) -> ugml, padx, fwft
  * jptl (61)
  * ugml (68) -> gyxo, ebii, jptl
  * gyxo (61)
  * cntj (57)
  * ...then you would be able to recreate the structure of the towers that looks like this:
  *
  *                 gyxo
  *               /
  *          ugml - ebii
  *        /      \
  *       |         jptl
  *       |
  *       |         pbga
  *      /        /
  * tknk --- padx - havc
  *      \        \
  *       |         qoyq
  *       |
  *       |         ktlj
  *        \      /
  *          fwft - cntj
  *               \
  *                 xhth
  * In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)
  *
  * Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?
  *
  * --- Part Two ---
  *
  * The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's fixed, they're stuck here.
  *
  * For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.
  *
  * In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same weight, and they do: 61.
  *
  * However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match. This means that the following sums must all be the same:
  *
  * ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
  * padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
  * fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
  * As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.
  *
  * Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?
  *
  */
object Day7 extends App {
  final case class Program(name: String, weight: Int, otherPrograms: Seq[String])

  def solutionPart1(input: Seq[Program]): String = {
    val (allParents, allSiblings) = input.foldLeft((Set.empty[String], Set.empty[String])) {
      case ((parents, siblings), program) => (parents + program.name, siblings ++ program.otherPrograms)
    }
    val programsWithNoSiblings = allParents -- allSiblings
    assert(programsWithNoSiblings.size == 1, "Found multiple roots of program tree")
    programsWithNoSiblings.head
  }

  // Playing around with cats
  def solutionPart1WithCats(input: Seq[Program]): String = {
    val programAndChildNames = input.map(program => (Set(program.name), program.otherPrograms.toSet))
    val (allParents, allSiblings) = Monoid[(Set[String], Set[String])].combineAll(programAndChildNames)
    val programsWithNoSiblings = allParents -- allSiblings
    assert(programsWithNoSiblings.size == 1, "Found multiple roots of program tree")
    programsWithNoSiblings.head
  }

  def solutionPart2(input: Seq[Program]): Int = {
    val rootProgramName = solutionPart1(input)

    val treeStructure = input.collect { case Program(name, _, head :: tail) => name -> NonEmptyList(head, tail) }.toMap
    val weights = input.map(program => program.name -> program.weight).toMap

    def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
      override def apply(key: I) = getOrElseUpdate(key, f(key))
    }

    lazy val calculateWeightForNode: (String => Int) = memoize { (node: String) =>
      treeStructure
        .get(node)
        .map(_.map(calculateWeightForNode).combineAll + weights.getOrElse(node, 0))
        .getOrElse(weights.getOrElse(node, 0))
    }

    @tailrec
    def findAnomaly(node: String, delta: Int): Int = {
      val childWeights = treeStructure(node).map(node => node -> calculateWeightForNode(node))
      val weightStatistics = childWeights.groupBy(_._2).mapValues(_.size)
      weightStatistics.keys.toList match {
        // There is only 1 weight for the children. It means the anomaly is in this node, so we return
        case one :: Nil => weights(node) + delta
        // There are 2 separate weights for children, one of them leads to the anomaly, let's recurse
        case one :: two :: Nil =>
          val anomalyWeight = weightStatistics.minBy(_._2)._1 // least common weight
          val normalWeight = weightStatistics.maxBy(_._2)._1 // most common weight
          val childWithAnomaly = childWeights.find(_._2 === anomalyWeight).map(_._1).get
          findAnomaly(childWithAnomaly, normalWeight - anomalyWeight)
        // More than 2 anomalies
        case _ => throw new IllegalStateException("Multiple anomalies found")
      }
    }

    findAnomaly(rootProgramName, 0)
  }


  def parseInputLine(programLine: String): Program = {
    val programInfo :: programNamesAbove = programLine.split(" -> ").toList
    val programName :: weightWithParens :: Nil = programInfo.split(' ').toList
    val programNames = programNamesAbove.headOption.map(_.split(", ").toList.map(_.trim)).getOrElse(Nil)
    Program(programName.trim, weightWithParens.stripPrefix("(").stripSuffix(")").toInt, programNames)
  }

  val problemInput = Source.fromResource("day7.txt").getLines().map(parseInputLine).toList
  println(solutionPart1(problemInput))
  println(solutionPart1WithCats(problemInput))
  println(solutionPart2(problemInput))
}
