package com.github.krivachy.advent

import scala.io.Source

object Day24 {

  sealed trait Tree
  final case class Node(subTrees: Seq[Tree]) extends Tree
  final case class Leaf(components: Vector[Component]) extends Tree

  final case class Component(a: Int, b: Int) {
    def canConnect(port: Int): Boolean = a == port || b == port
    def otherPort(port: Int): Int = if (a == port) b else a
    def strength: Int = a + b
  }

  def solutionPart1(components: Vector[Component]): Int = {
    def recurse(currentOpenPort: Int, remainingComponents: Set[Component]): Vector[Vector[Component]] = {
      val connectableComponents = remainingComponents.filter(_.canConnect(currentOpenPort))
      (for {
        component <- connectableComponents.toSeq
        nextOpenPort = component.otherPort(currentOpenPort)
        bridge <- Vector.empty[Vector[Component]] ++ recurse(nextOpenPort, remainingComponents - component)
      } yield bridge :+ component).toVector
    }
    val allBridges = recurse(0, components.toSet)
    allBridges.map(_.map(_.strength).sum).max
  }

  def parseInputLine(line: String): Component = {
    line.split("/").toList match {
      case a :: b :: Nil => Component(a.toInt, b.toInt)
      case _ => throw new IllegalArgumentException("Unknown line: " + line)
    }
  }
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day24.txt").getLines.map(parseInputLine).toVector
    println(solutionPart1(input))
  }

}
