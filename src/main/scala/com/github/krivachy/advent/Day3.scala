package com.github.krivachy.advent

import scala.annotation.tailrec


/**
  * --- Day 3: Spiral Memory ---
  *
  * You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
  *
  * Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
  *
  * 17  16  15  14  13
  * 18   5   4   3  12
  * 19   6   1   2  11
  * 20   7   8   9  10
  * 21  22  23---> ...
  * While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
  *
  * For example:
  *
  * Data from square 1 is carried 0 steps, since it's at the access port.
  * Data from square 12 is carried 3 steps, such as: down, left, left.
  * Data from square 23 is carried only 2 steps: up twice.
  * Data from square 1024 must be carried 31 steps.
  * How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
  *
  * --- Part Two ---
  *
  * As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
  *
  * So, the first few squares' values are chosen as follows:
  *
  * Square 1 starts with the value 1.
  * Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
  * Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
  * Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
  * Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
  * Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
  *
  * 147  142  133  122   59
  * 304    5    4    2   57
  * 330   10    1    1   54
  * 351   11   23   25   26
  * 362  747  806--->   ...
  * What is the first value written that is larger than your puzzle input?
  */
object Day3 extends App {

  final case class Location(distanceFromCenter: Int, oneAxisNumber: Int, otherAxisNumber: Int)

  /**
    * Idea is that we circle along the axis numbers outwards and find which two axis numbers the
    * number we're trying to find is between.
    *
    * axis number = numbers along the x and y of a typical coordinate system
    *
    * Example:
    *
    *       1 x x x x x j
    *       x
    *       x
    *       x
    *       x
    *       i         n
    *
    * Where n is the number we're trying to find then we jump between i and j and see that n is
    * somewhere inbetween. The number of x's denote the distanceFromCenter. Every time there's an
    * increase in the distance from center we need to increment numbers according to rules
    */
  def findLocationInGrid(numberToFind: Int): Location = {
    @tailrec
    def findByCirclingAxisNumbersOutwards(distanceFromCenter: Int, numberOfAxisVisited: Int, currentAxisNumber: Int, currentDistanceBetweenAxisNumbers: Int, previousAxisNumber: Int): Location = {
      if (numberToFind >= previousAxisNumber && numberToFind <= currentAxisNumber) {
        Location(distanceFromCenter, previousAxisNumber, currentAxisNumber)
      } else {
        if (numberOfAxisVisited == 4) {
          // We're rounding a corner here, therefore increasing out distance from center!
          findByCirclingAxisNumbersOutwards(
            distanceFromCenter = distanceFromCenter + 1,
            numberOfAxisVisited = 1,
            // We need + 1 due to going around a corner which adds an extra number between
            // the axis numbers
            currentAxisNumber = currentAxisNumber + currentDistanceBetweenAxisNumbers + 1,
            currentDistanceBetweenAxisNumbers = currentDistanceBetweenAxisNumbers + 2,
            previousAxisNumber = currentAxisNumber
          )
        } else {
          findByCirclingAxisNumbersOutwards(
            distanceFromCenter = distanceFromCenter,
            numberOfAxisVisited = numberOfAxisVisited + 1,
            currentAxisNumber = currentAxisNumber + currentDistanceBetweenAxisNumbers,
            currentDistanceBetweenAxisNumbers = currentDistanceBetweenAxisNumbers,
            previousAxisNumber = currentAxisNumber
          )
        }
      }
    }

    if (numberToFind == 1) Location(0, 0, 1)
    else findByCirclingAxisNumbersOutwards(
      distanceFromCenter = 1,
      numberOfAxisVisited = 1,
      currentAxisNumber = 2,
      currentDistanceBetweenAxisNumbers = 2,
      previousAxisNumber = 1
    )
  }


  def solutionPart1(numberToLookup: Int): Int = {
    val location = findLocationInGrid(numberToLookup)
    val distanceFromAxis = Math.min(Math.abs(location.oneAxisNumber - numberToLookup), Math.abs(location.otherAxisNumber - numberToLookup))
    location.distanceFromCenter + distanceFromAxis
  }


  val input = 347991
  println(solutionPart1(input))
}
