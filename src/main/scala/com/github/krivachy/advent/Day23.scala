package com.github.krivachy.advent

import scala.annotation.tailrec
import scala.io.Source

/**
  * --- Day 23: Coprocessor Conflagration ---
  * You decide to head directly to the CPU and fix the printer from there. As you get close, you find an experimental coprocessor doing so much work that the local programs are afraid it will halt and catch fire. This would cause serious issues for the rest of the computer, so you head in and see what you can do.
  *
  * The code it's running seems to be a variant of the kind you saw recently on that tablet. The general functionality seems very similar, but some of the instructions are different:
  *
  * set X Y sets register X to the value of Y.
  * sub X Y decreases register X by the value of Y.
  * mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
  * jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
  * Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.
  *
  * The coprocessor is currently set to some kind of debug mode, which allows for testing, but prevents it from doing any meaningful work.
  *
  * If you run the program (your puzzle input), how many times is the mul instruction invoked?
  *
  * --- Part Two ---
  * Now, it's time to fix the problem.
  *
  * The debug mode switch is wired directly to register a. You flip the switch, which makes register a now start at 1 when the program is executed.
  *
  * Immediately, the coprocessor begins to overheat. Whoever wrote this program obviously didn't choose a very efficient implementation. You'll need to optimize the program if it has any hope of completing before Santa needs that printer working.
  *
  * The coprocessor's ultimate goal is to determine the final value left in register h once the program completes. Technically, if it had that... it wouldn't even need to run the program.
  *
  * After setting register a to 1, if the program were to run to completion, what value would be left in register h?
  */
object Day23 {

  sealed trait Value
  final case class Constant(value: Long) extends Value
  final case class Register(register: String) extends Value

  sealed trait Instruction
  final case class Set(register: String, value: Value) extends Instruction
  final case class Sub(register: String, value: Value) extends Instruction
  final case class Mul(register: String, value: Value) extends Instruction
  final case class Jnz(value: Value, jumpOffset: Value) extends Instruction

  private def resolveValueWithRegister(registers: Map[String, Long])(value: Value): Long = {
    value match {
      case Constant(v)   => v
      case Register(reg) => registers.getOrElse(reg, 0L)
    }
  }

  def solutionPart1(instructions: Vector[Instruction]): Long = {
    @tailrec
    def recurse(instructionPointer: Int, registers: Map[String, Long], multiplicationCount: Int): Long = {
      if (instructionPointer >= instructions.length || instructionPointer < 0) {
        multiplicationCount
      } else {
        val resolveValue = resolveValueWithRegister(registers) _
        def operateOnRegister(register: String)(value: Value)(operation: (Long, Long) => Long): Map[String, Long] = {
          registers.updated(register,
            operation.apply(registers.getOrElse(register, 0),
              resolveValue(value)))
        }
        instructions(instructionPointer) match {
          case Set(register, value) =>
            recurse(
              instructionPointer + 1,
              operateOnRegister(register)(value) { case (_, current) => current },
              multiplicationCount
            )
          case Sub(register, value) =>
            recurse(
              instructionPointer + 1,
              operateOnRegister(register)(value)(_ - _),
              multiplicationCount
            )
          case Mul(register, value) =>
            recurse(
              instructionPointer + 1,
              operateOnRegister(register)(value)(_ * _),
              multiplicationCount + 1
            )
          case Jnz(value, jumpOffset) =>
            if (resolveValue(value) != 0) {
              recurse(instructionPointer + resolveValue(jumpOffset).toInt,
                registers,
                multiplicationCount)
            } else {
              recurse(instructionPointer + 1, registers, multiplicationCount)
            }
        }
      }
    }
    recurse(0, Map.empty, 0)
  }

  private def nonPrime(n: Int) = (2 until n - 1).exists(n % _ == 0)
  def solutionPart2(input: Int): Int = {
    val start = input * 100 + 100000
    val end = start + 17001
    val step = 17
    Range(start, end, step).foldLeft(0) {
      case (nonPrimeCount, next) =>
        if (nonPrime(next)) nonPrimeCount + 1
        else nonPrimeCount
    }
  }

  def parseValue(valueStr: String): Value = {
    if (valueStr.matches("^-?[\\d]+$")) Constant(valueStr.toLong)
    else Register(valueStr)
  }

  def parseInstructionLine(line: String): Instruction = {
    line.split(" ").toList match {
      case "set" :: register :: value :: Nil => Set(register, parseValue(value))
      case "sub" :: register :: value :: Nil => Sub(register, parseValue(value))
      case "mul" :: register :: value :: Nil => Mul(register, parseValue(value))
      case "jnz" :: value :: offset :: Nil   => Jnz(parseValue(value), parseValue(offset))
      case _ =>
        throw new IllegalArgumentException("Unknown command line: " + line)
    }
  }

  def main(args: Array[String]): Unit = {
    val problemInput = Source
      .fromResource("day23.txt")
      .getLines
      .map(parseInstructionLine)
      .toVector
    println(solutionPart1(problemInput))
    println(solutionPart2(problemInput.head.asInstanceOf[Set].value.asInstanceOf[Constant].value.toInt))
  }
}
