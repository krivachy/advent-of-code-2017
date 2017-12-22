package com.github.krivachy.advent

import scala.annotation.tailrec
import scala.io.Source

/**
  * --- Day 22: Sporifica Virus ---
  * Diagnostics indicate that the local grid computing cluster has been contaminated with the Sporifica Virus. The grid computing cluster is a seemingly-infinite two-dimensional grid of compute nodes. Each node is either clean or infected by the virus.
  *
  * To prevent overloading the nodes (which would render them useless to the virus) or detection by system administrators, exactly one virus carrier moves through the network, infecting or cleaning nodes as it moves. The virus carrier is always located on a single node in the network (the current node) and keeps track of the direction it is facing.
  *
  * To avoid detection, the virus carrier works in bursts; in each burst, it wakes up, does some work, and goes back to sleep. The following steps are all executed in order one time each burst:
  *
  * If the current node is infected, it turns to its right. Otherwise, it turns to its left. (Turning is done in-place; the current node does not change.)
  * If the current node is clean, it becomes infected. Otherwise, it becomes cleaned. (This is done after the node is considered for the purposes of changing direction.)
  * The virus carrier moves forward one node in the direction it is facing.
  * Diagnostics have also provided a map of the node infection status (your puzzle input). Clean nodes are shown as .; infected nodes are shown as #. This map only shows the center of the grid; there are many more nodes beyond those shown, but none of them are currently infected.
  *
  * The virus carrier begins in the middle of the map facing up.
  *
  * For example, suppose you are given a map like this:
  *
  * ..#
  * #..
  * ...
  * Then, the middle of the infinite grid looks like this, with the virus carrier's position marked with [ ]:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . # . . .
  * . . . #[.]. . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * The virus carrier is on a clean node, so it turns left, infects the node, and moves left:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . # . . .
  * . . .[#]# . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * The virus carrier is on an infected node, so it turns right, cleans the node, and moves up:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . .[.]. # . . .
  * . . . . # . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * Four times in a row, the virus carrier finds a clean, infects it, turns left, and moves forward, ending in the same place and still facing up:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . #[#]. # . . .
  * . . # # # . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * Now on the same node as before, it sees an infection, which causes it to turn right, clean the node, and move forward:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . # .[.]# . . .
  * . . # # # . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * After the above actions, a total of 7 bursts of activity had taken place. Of them, 5 bursts of activity caused an infection.
  *
  * After a total of 70, the grid looks like this, with the virus carrier facing up:
  *
  * . . . . . # # . .
  * . . . . # . . # .
  * . . . # . . . . #
  * . . # . #[.]. . #
  * . . # . # . . # .
  * . . . . . # # . .
  * . . . . . . . . .
  * . . . . . . . . .
  * By this time, 41 bursts of activity caused an infection (though most of those nodes have since been cleaned).
  *
  * After a total of 10000 bursts of activity, 5587 bursts will have caused an infection.
  *
  * Given your actual map, after 10000 bursts of activity, how many bursts cause a node to become infected? (Do not count nodes that begin infected.)
  *
  * --- Part Two ---
  * As you go to remove the virus from the infected nodes, it evolves to resist your attempt.
  *
  * Now, before it infects a clean node, it will weaken it to disable your defenses. If it encounters an infected node, it will instead flag the node to be cleaned in the future. So:
  *
  * Clean nodes become weakened.
  * Weakened nodes become infected.
  * Infected nodes become flagged.
  * Flagged nodes become clean.
  * Every node is always in exactly one of the above states.
  *
  * The virus carrier still functions in a similar way, but now uses the following logic during its bursts of action:
  *
  * Decide which way to turn based on the current node:
  * If it is clean, it turns left.
  * If it is weakened, it does not turn, and will continue moving in the same direction.
  * If it is infected, it turns right.
  * If it is flagged, it reverses direction, and will go back the way it came.
  * Modify the state of the current node, as described above.
  * The virus carrier moves forward one node in the direction it is facing.
  * Start with the same map (still using . for clean and # for infected) and still with the virus carrier starting in the middle and facing up.
  *
  * Using the same initial state as the previous example, and drawing weakened as W and flagged as F, the middle of the infinite grid looks like this, with the virus carrier's position again marked with [ ]:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . # . . .
  * . . . #[.]. . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * This is the same as before, since no initial nodes are weakened or flagged. The virus carrier is on a clean node, so it still turns left, instead weakens the node, and moves left:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . # . . .
  * . . .[#]W . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * The virus carrier is on an infected node, so it still turns right, instead flags the node, and moves up:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . .[.]. # . . .
  * . . . F W . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * This process repeats three more times, ending on the previously-flagged node and facing right:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . W W . # . . .
  * . . W[F]W . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * Finding a flagged node, it reverses direction and cleans the node:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . W W . # . . .
  * . .[W]. W . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * The weakened node becomes infected, and it continues in the same direction:
  *
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . W W . # . . .
  * .[.]# . W . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * . . . . . . . . .
  * Of the first 100 bursts, 26 will result in infection. Unfortunately, another feature of this evolved virus is speed; of the first 10000000 bursts, 2511944 will result in infection.
  *
  * Given your actual map, after 10000000 bursts of activity, how many bursts cause a node to become infected? (Do not count nodes that begin infected.)
  */
object Day22 {
  /*
    COMMON
   */
  sealed abstract class Direction(val x: Int, val y: Int)
  object Direction {
    def reverse(direction: Direction): Direction = direction match {
      case Up => Down
      case Down => Up
      case Left => Right
      case Right => Left
    }
    def turnRight(direction: Direction): Direction = direction match {
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up
    }
    def turnLeft(direction: Direction): Direction = direction match {
      case Up => Left
      case Left => Down
      case Down => Right
      case Right => Up
    }
  }
  final case object Up extends Direction(-1, 0)
  final case object Down extends Direction(1, 0)
  final case object Left extends Direction(0, -1)
  final case object Right extends Direction(0, 1)

  final case class Position(x: Int, y: Int) {
    def move(direction: Direction): Position = Position(x + direction.x, y + direction.y)
    def coords: (Int, Int) = (x, y)
  }

  /*
    PART 1
   */

  final case class VirusState(map: Map[(Int, Int), Char], position: Position, direction: Direction, infectedCount: Int = 0) {
    def isInfectedAtCurrentPosition: Boolean = map.get(position.coords).contains('#')
    def turnLeft: VirusState = this.copy(direction = Direction.turnLeft(direction))
    def turnRight: VirusState = this.copy(direction = Direction.turnRight(direction))
    def moveForward: VirusState = this.copy(position = position.move(direction))
    def infect: VirusState = this.copy(map = map.updated(position.coords, '#'), infectedCount = infectedCount + 1)
    def clean: VirusState = this.copy(map = map.updated(position.coords, '.'))
  }

  def solutionPart1(bursts: Int, initialGrid: Vector[Vector[Char]]): Int = {
    val initialMap = transformGridIntoMap(initialGrid)
    val initialState = VirusState(initialMap, Position(0, 0), Up)
    @tailrec def loop(state: VirusState, i: Int): VirusState = {
      if (i < bursts) loop(runBurst(state), i + 1)
      else state
    }
    loop(initialState, 0).infectedCount
  }

  private def runBurst(virusState: VirusState): VirusState = {
    if (virusState.isInfectedAtCurrentPosition)
      virusState.turnRight.clean.moveForward
    else
      virusState.turnLeft.infect.moveForward
  }

  /*
    PART 2
   */

  final case class EvolvedVirusState(map: Map[(Int, Int), Char], position: Position, direction: Direction, infectedCount: Int = 0) {
    def currentPosition: Char = map.getOrElse(position.coords, '.')
    def turnLeft: EvolvedVirusState = this.copy(direction = Direction.turnLeft(direction))
    def turnRight: EvolvedVirusState = this.copy(direction = Direction.turnRight(direction))
    def reverse: EvolvedVirusState = this.copy(direction = Direction.reverse(direction))
    def moveForward: EvolvedVirusState = this.copy(position = position.move(direction))
    def infect: EvolvedVirusState = updateCurrentPosition('#').copy(infectedCount = infectedCount + 1)
    def flag: EvolvedVirusState = updateCurrentPosition('F')
    def clean: EvolvedVirusState = updateCurrentPosition('.')
    def weaken: EvolvedVirusState = updateCurrentPosition('W')
    private def updateCurrentPosition(char: Char) =
      this.copy(map = map.updated(position.coords, char))
  }

  def solutionPart2(bursts: Int, initialGrid: Vector[Vector[Char]]): Int = {
    val initialMap = transformGridIntoMap(initialGrid)
    val initialState = EvolvedVirusState(initialMap, Position(0, 0), Up)
    @tailrec def loop(state: EvolvedVirusState, i: Int): EvolvedVirusState = {
      if (i < bursts) loop(runEvolvedBurst(state), i + 1)
      else state
    }
    loop(initialState, 0).infectedCount
  }

  def runEvolvedBurst(evolvedVirusState: EvolvedVirusState): EvolvedVirusState = {
    evolvedVirusState.currentPosition match {
      case '.' =>
        evolvedVirusState.weaken.turnLeft.moveForward
      case 'W' =>
        evolvedVirusState.infect.moveForward
      case '#' =>
        evolvedVirusState.flag.turnRight.moveForward
      case 'F' =>
        evolvedVirusState.clean.reverse.moveForward
    }
  }

  /*
    INPUT PROCESSING
   */

  private def transformGridIntoMap(grid: Vector[Vector[Char]]): Map[(Int, Int), Char] = {
    val centerX = grid.length / 2
    val centerY = grid.head.length / 2
    (for {
      (row, xIndex) <- grid.zipWithIndex
      (cell, yIndex) <- row.zipWithIndex
    } yield (xIndex - centerX, yIndex - centerY) -> cell).toMap
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day22.txt").getLines.map(_.toVector).toVector
    println(solutionPart1(10000, input))
    println(solutionPart2(10000000, input))
  }

}
