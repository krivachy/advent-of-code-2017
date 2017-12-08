package com.github.krivachy.advent

import scala.io.Source

/**
  * --- Day 8: I Heard You Like Registers ---
  *
  * You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like you to compute the result of a series of unusual register instructions.
  *
  * Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:
  *
  * b inc 5 if a > 1
  * a inc 1 if b < 5
  * c dec -10 if a >= 1
  * c inc -20 if c == 10
  * These instructions would be processed as follows:
  *
  * Because a starts at 0, it is not greater than 1, and so b is not modified.
  * a is increased by 1 (to 1) because b is less than 5 (it is 0).
  * c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
  * c is increased by -20 (to -10) because c is equal to 10.
  * After this process, the largest value in any register is 1.
  *
  * You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.
  *
  * What is the largest value in any register after completing the instructions in your puzzle input?
  *
  * --- Part Two ---
  *
  * To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).
  */
object Day8 extends App {

  final case class Instruction(register: String, incOrDec: Int, amount: Int, conditionalRegister: String, condition: Int => Boolean)


  def solutionPart1(instructions: Seq[Instruction]): Int = {
    val emptyRegisters = Map.empty[String, Int]
    val finalRegisterState = instructions.foldLeft(emptyRegisters) {
      case (registers, Instruction(register, incOrDec, amount, conditionalRegister, condition)) =>
        if (condition(registers.getOrElse(conditionalRegister, 0))) {
          val previousAmount = registers.getOrElse(register, 0)
          registers.updated(register, previousAmount + (incOrDec * amount))
        } else {
          registers
        }
    }
    finalRegisterState.values.max
  }

  def solutionPart2(instructions: Seq[Instruction]): Int = {
    // values: (currentRegisterValue, maxRegisterValue)
    val emptyRegisters = Map.empty[String, (Int, Int)]
    val finalRegisterState = instructions.foldLeft(emptyRegisters) {
      case (registers, Instruction(register, incOrDec, amount, conditionalRegister, condition)) =>
        if (condition(registers.get(conditionalRegister).map(_._1).getOrElse(0))) {
          val (previousAmount, maxAmount) = registers.getOrElse(register, (0, 0))
          val newAmount = previousAmount + (incOrDec * amount)
          registers.updated(register, (newAmount, Math.max(maxAmount, newAmount)))
        } else {
          registers
        }
    }
    finalRegisterState.values.map(_._2).max
  }


  def parseInputLine(inputLine: String): Instruction = {
    inputLine.split(" ").toList match {
      case register :: incOrDecString :: amount :: "if" :: conditionalRegister :: sign :: conditionIntString :: Nil =>
        val incOrDec = incOrDecString match {
          case "inc" => 1
          case "dec" => -1
          case other => throw new IllegalArgumentException("Unknown input: " + other)
        }
        Instruction(
          register,
          incOrDec,
          amount.toInt,
          conditionalRegister,
          registerInt => {
            val conditionInt = conditionIntString.toInt
            sign match {
              case ">" => registerInt > conditionInt
              case "<" => registerInt < conditionInt
              case ">=" => registerInt >= conditionInt
              case "<=" => registerInt <= conditionInt
              case "==" => registerInt == conditionInt
              case "!=" => registerInt != conditionInt
              case other => throw new IllegalArgumentException("Unknown sign: " + other)
            }
          }
        )
      case _ => throw new IllegalArgumentException("Input not well formed")
    }
  }

  val problemInput = Source.fromResource("day8.txt").getLines().map(parseInputLine).toList
  println(solutionPart1(problemInput))
  println(solutionPart2(problemInput))
}
