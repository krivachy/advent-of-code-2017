package com.github.krivachy.advent

import scala.io.Source
import Function.tupled
import scala.annotation.tailrec

/**
  * --- Day 20: Particle Swarm ---
  * Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate too many particles, and it won't be able to finish them all in time to render the next frame at this rate.
  *
  * It transmits to you a buffer (your puzzle input) listing each particle in order (starting with particle 0, then particle 1, particle 2, and so on). For each particle, it provides the X, Y, and Z coordinates for the particle's position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.
  *
  * Each tick, all particles are updated simultaneously. A particle's properties are updated in the following order:
  *
  * Increase the X velocity by the X acceleration.
  * Increase the Y velocity by the Y acceleration.
  * Increase the Z velocity by the Z acceleration.
  * Increase the X position by the X velocity.
  * Increase the Y position by the Y velocity.
  * Increase the Z position by the Z velocity.
  * Because of seemingly tenuous rationale involving z-buffering, the GPU would like to know which particle will stay closest to position <0,0,0> in the long term. Measure this using the Manhattan distance, which in this situation is simply the sum of the absolute values of a particle's X, Y, and Z position.
  *
  * For example, suppose you are only given two particles, both of which stay entirely on the X-axis (for simplicity). Drawing the current states of particles 0 and 1 (in that order) with an adjacent a number line and diagram of current X positions (marked in parenthesis), the following would take place:
  *
  * p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
  *
  * p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
  *
  * p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
  *
  * p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)
  * At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will stay closest.
  *
  * Which particle will stay closest to position <0,0,0> in the long term?
  *
  * The first half of this puzzle is complete! It provides one gold star: *
  *
  * --- Part Two ---
  * To simplify the problem further, the GPU would like to remove any particles that collide. Particles collide if their positions ever exactly match. Because particles are updated simultaneously, more than two particles can collide at the same time and place. Once particles collide, they are removed and cannot collide with anything else after that tick.
  *
  * For example:
  *
  * p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
  * p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  * p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
  * p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
  *
  * p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
  * p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  * p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
  * p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>
  *
  * p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
  * p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  * p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
  * p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>
  *
  * ------destroyed by collision------
  * ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
  * ------destroyed by collision------                      (3)
  * p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
  * In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. On the next tick, particle 3 passes through unharmed.
  *
  * How many particles are left after all collisions are resolved?
  */
object Day20 {
  /*
   COMMON DOMAIN
   */
  final case class Coord(x: Long, y: Long, z: Long) {
    def +(c: Coord): Coord = Coord(x + c.x, y + c.y, z + c.z)
    def manhattanDistanceFrom(c: Coord): Long = (x - c.x).abs + (y - c.y).abs + (z - c.z).abs
  }
  final case class Particle(position: Coord,
                            velocity: Coord,
                            acceleration: Coord,
                            live: Boolean = true) {
    def collided: Particle = this.copy(live = false)
  }

  private def tickParticle(particle: Particle): Particle = {
    if (particle.live) {
      val newVelocity = particle.velocity + particle.acceleration
      particle.copy(
        position = particle.position + newVelocity,
        velocity = newVelocity
      )
    } else particle
  }
  private val origo = Coord(0, 0, 0)
  private def distanceFromOrigo(particle: Particle): Long = particle.position.manhattanDistanceFrom(origo)

  /*
   PART 1
   */

  def solutionPart1(tick0: Vector[Particle]): Int = {
    @tailrec
    def loopUntilAllMovingAwayFromOrigo(particles: Vector[Particle], previousDistances: Vector[Long]): Int = {
      val newTick = particles.map(tickParticle)
      val newDistances = newTick.map(distanceFromOrigo)
      val diffInDistances = newDistances.zip(previousDistances).map { case (n, p) => n - p }
      // Check to see if the distances are all increasing from the origo
      // If they are then we just choose the one with the lowest acceleration as in t=+infinity
      // that will definitely be the closest
      if (diffInDistances.forall(_ > 0)) {
        newTick.indexOf(newTick.minBy(_.acceleration.manhattanDistanceFrom(origo)))
      } else {
        loopUntilAllMovingAwayFromOrigo(newTick, newDistances)
      }
    }
    loopUntilAllMovingAwayFromOrigo(tick0, tick0.map(distanceFromOrigo))
  }

  /*
   PART 2
   */

  def solutionPart2(tick0: Vector[Particle]): Int = {
    // meh, this probably doesn't cover all edge cases, but works for my input
    @tailrec
    def loopUntilDistancesAlwaysGrow(particleState: Vector[Particle], previousDistances: Map[(Int, Int), Long]): Int = {
      val newParticleStateSimulation = particleState.map(tickParticle)
      val positionsOfCollision = getPositionsOfCollision(newParticleStateSimulation)
      val newParticleState = newParticleStateSimulation.map(markParticleDeadIfCollided(positionsOfCollision))
      val newPairwiseDistances = calculatePairwiseDistances(newParticleState, previousDistances.keys.toSeq)
      val particleIndicesThatGetCloserToEachOther = newPairwiseDistances.filter {
        case (indexPair, newDistance) => newDistance - previousDistances(indexPair) < 0
      }.keySet
      if (particleIndicesThatGetCloserToEachOther.isEmpty) newParticleState.count(_.live)
      else {
        val distancesOfInterest = newPairwiseDistances.filterKeys(particleIndicesThatGetCloserToEachOther.contains)
        loopUntilDistancesAlwaysGrow(newParticleState, distancesOfInterest)
      }
    }
    loopUntilDistancesAlwaysGrow(tick0, calculateInitialPairwiseDistances(tick0))
  }

  private def markParticleDeadIfCollided(collisionPositions: Set[Coord])(particle: Particle): Particle = {
    if (collisionPositions(particle.position)) particle.collided else particle
  }

  private def getPositionsOfCollision(particles: Vector[Particle]): Set[Coord] = {
    val liveParticlesByPosition = particles.filter(_.live).groupBy(_.position)
    liveParticlesByPosition.filter(_._2.length > 1).keySet
  }

  private def calculatePairwiseDistances(particles: Vector[Particle], pairs: Seq[(Int, Int)]): Map[(Int, Int), Long] = {
    pairs.map(tupled((first, second) => (first, second) -> particles(first).position.manhattanDistanceFrom(particles(second).position))).toMap
  }

  private def calculateInitialPairwiseDistances(particles: Vector[Particle]): Map[(Int, Int), Long] = {
    val pairwiseIndices = particles
      .zipWithIndex
      .collect { case (particle, index) if particle.live => index }
      .combinations(2)
      .map { case index1 +: index2 +: _ => (index1, index2) }
    calculatePairwiseDistances(particles, pairwiseIndices.toSeq)
  }

  /*
   PARSING OF INPUT
   */

  private val ParticleRegex = """p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r
  def parseInputLine(line: String): Particle = {
    line match {
      case ParticleRegex(px, py, pz, vx, vy, vz, ax, ay, az) =>
        Particle(
          Coord(px.toLong, py.toLong, pz.toLong),
          Coord(vx.toLong, vy.toLong, vz.toLong),
          Coord(ax.toLong, ay.toLong, az.toLong)
        )
      case _ => throw new IllegalArgumentException("Unknown line: " + line)
    }
  }

  /*
   MAIN
   */

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day20.txt").getLines.map(parseInputLine).toVector
    println(solutionPart1(input))
    println(solutionPart2(input))
  }
}
