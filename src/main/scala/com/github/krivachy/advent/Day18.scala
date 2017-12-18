package com.github.krivachy.advent

import scala.collection.immutable.Queue
import scala.io.Source

/**
  * --- Day 18: Duet ---
  * You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure out what the instructions mean on your own.
  *
  * It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that can each hold a single integer. You suppose each register should start with a value of 0.
  *
  * There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:
  *
  * snd X plays a sound with a frequency equal to the value of X.
  * set X Y sets register X to the value of Y.
  * add X Y increases register X by the value of Y.
  * mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
  * mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
  * rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
  * jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
  * Many of the instructions can take either a register (a single letter) or a number. The value of a register is the integer it contains; the value of a number is that number.
  *
  * After each jump instruction, the program continues with the instruction to which the jump jumped. After any other instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program terminates it.
  *
  * For example:
  *
  * set a 1
  * add a 2
  * mul a a
  * mod a 5
  * snd a
  * set a 0
  * rcv a
  * jgz a -1
  * set a 1
  * jgz a -2
  * The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a value of 4.
  * Then, a sound with frequency 4 (the value of a) is played.
  * After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
  * Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump, which jumps again to the rcv, which ultimately triggers the recover operation.
  * At the time the recover operation is executed, the frequency of the last sound played is 4.
  *
  * What is the value of the recovered frequency (the value of the most recently played sound) the first time a rcv instruction is executed with a non-zero value?
  *
  * --- Part Two ---
  * As you congratulate yourself for a job well done, you notice that the documentation has been on the back of the tablet this entire time. While you actually got most of the instructions correct, there are a few key differences. This assembly code isn't about sound at all - it's meant to be run twice at the same time.
  *
  * Each running copy of the program has its own set of registers and follows the code independently - in fact, the programs don't even necessarily run at the same speed. To coordinate, they use the send (snd) and receive (rcv) instructions:
  *
  * snd X sends the value of X to the other program. These values wait in a queue until that program is ready to receive them. Each program has its own message queue, so a program can never receive a message it sent.
  * rcv X receives the next value and stores it in register X. If no values are in the queue, the program waits for a value to be sent to it. Programs do not continue to the next instruction until they have received a value. Values are received in the order they are sent.
  * Each program also has its own program ID (one 0 and the other 1); the register p should begin with this value.
  *
  * For example:
  *
  * snd 1
  * snd 2
  * snd p
  * rcv a
  * rcv b
  * rcv c
  * rcv d
  * Both programs begin by sending three values to the other. Program 0 sends 1, 2, 0; program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and stores it in a, receives another value (both 2) and stores it in b, and then each receives the program ID of the other program (program 0 receives 1; program 1 receives 0) and stores it in c. Each program now sees a different value in its own copy of register c.
  *
  * Finally, both programs try to rcv a fourth time, but no data is waiting for either of them, and they reach a deadlock. When this happens, both programs terminate.
  *
  * It should be noted that it would be equally valid for the programs to run at different speeds; for example, program 0 might have sent all three values and then stopped at the first rcv before program 1 executed even its first instruction.
  *
  * Once both of your programs have terminated (regardless of what caused them to do so), how many times did program 1 send a value?
  */
object Day18 {

  sealed trait Value
  final case class Constant(value: Long) extends Value
  final case class Register(register: String) extends Value

  sealed trait Instruction
  final case class Snd(register: String) extends Instruction
  final case class Set(register: String, value: Value) extends Instruction
  final case class Add(register: String, value: Value) extends Instruction
  final case class Mul(register: String, value: Value) extends Instruction
  final case class Mod(register: String, value: Value) extends Instruction
  final case class Rcv(register: String) extends Instruction
  final case class Jgz(value: Value, jumpOffset: Value) extends Instruction

  private def resolveValueWithRegister(registers: Map[String, Long])(value: Value): Long = {
    value match {
      case Constant(v)   => v
      case Register(reg) => registers.getOrElse(reg, 0L)
    }
  }

  def solutionPart1(instructions: Vector[Instruction]): Long = {
    def recurse(instructionPointer: Int,
                registers: Map[String, Long],
                lastSoundFrequency: Option[Long]): Long = {
      if (instructionPointer > instructions.length || instructionPointer < 0) {
        0
      } else {
        val resolveValue = resolveValueWithRegister(registers) _
        def operateOnRegister(register: String)(value: Value)(
            operation: (Long, Long) => Long): Long = {
          recurse(
            instructionPointer + 1,
            registers.updated(register,
                              operation.apply(registers.getOrElse(register, 0),
                                              resolveValue(value))),
            lastSoundFrequency
          )
        }
        def continueWithNextInstruction(): Long =
          recurse(instructionPointer + 1, registers, lastSoundFrequency)

        instructions(instructionPointer) match {
          case Snd(register) =>
            recurse(instructionPointer + 1,
                    registers,
                    Option(registers.getOrElse(register, 0)))
          case Set(register, value) =>
            operateOnRegister(register)(value) { case (_, current) => current }
          case Add(register, value) => operateOnRegister(register)(value)(_ + _)
          case Mul(register, value) => operateOnRegister(register)(value)(_ * _)
          case Mod(register, value) => operateOnRegister(register)(value)(_ % _)
          case Rcv(register) =>
            if (registers.getOrElse(register, 0) == 0) {
              continueWithNextInstruction()
            } else {
              lastSoundFrequency.getOrElse(0)
            }
          case Jgz(value, jumpOffset) =>
            if (resolveValue(value) > 0) {
              recurse(instructionPointer + resolveValue(jumpOffset).toInt,
                      registers,
                      lastSoundFrequency)
            } else {
              continueWithNextInstruction()
            }
        }
      }
    }
    recurse(0, Map.empty, Option.empty)
  }

  final case class ProgramState(instructionPointer: Int,
                                registers: Map[String, Long],
                                receiveQueue: Queue[Long],
                                sendQueue: Queue[Long],
                                sendCount: Int,
                                isBlocked: Boolean) {
    def nextInstructionFor(instructions: Seq[Instruction]): Option[Instruction] = {
      if (instructionPointer >= instructions.length || instructionPointer < 0) None
      else Option(instructions(instructionPointer))
    }

    def runNextStep(instructions: Seq[Instruction]): ProgramState = {
      nextInstructionFor(instructions) match {
        case None => this.copy(isBlocked = true)
        case Some(instruction) =>
          val resolveValue = resolveValueWithRegister(registers) _
          def setRegister(register: String)(value: Value): ProgramState = {
            this.copy(
              instructionPointer = instructionPointer + 1,
              registers = registers.updated(register, resolveValue(value))
            )
          }
          def updateRegister(register: String)(value: Value)(update: (Long, Long) => Long): ProgramState = {
            this.copy(
              instructionPointer = instructionPointer + 1,
              registers = registers.updated(
                register,
                update.apply(registers.getOrElse(register, 0), resolveValue(value)))
            )
          }
          instruction match {
            case Snd(register) =>
              this.copy(
                instructionPointer = instructionPointer + 1,
                sendCount = sendCount + 1,
                sendQueue = sendQueue.enqueue(resolveValue(Register(register)))
              )
            case Set(register, value) =>
              setRegister(register)(value)
            case Add(register, value) =>
              updateRegister(register)(value)(_ + _)
            case Mul(register, value) =>
              updateRegister(register)(value)(_ * _)
            case Mod(register, value) =>
              updateRegister(register)(value)(_ % _)
            case Rcv(register) =>
              receiveQueue.dequeueOption match {
                case Some((next, rest)) =>
                  setRegister(register)(Constant(next)).copy(receiveQueue = rest)
                case None =>
                  this.copy(isBlocked = true)
              }
            case Jgz(value, jumpOffset) =>
              val updatedIp = if (resolveValue(value) > 0L)
                instructionPointer + resolveValue(jumpOffset).toInt
              else
                instructionPointer + 1
              this.copy(instructionPointer = updatedIp)
          }
      }
    }
  }

  def solutionPart2(instructions: Vector[Instruction]): Int = {
    def isProgramBlocked(program: ProgramState): Boolean = {
      program.isBlocked || program.nextInstructionFor(instructions).isEmpty
    }

    def runProgramUntilBlocked(program: ProgramState): ProgramState = {
      Iterator.iterate(program)(_.runNextStep(instructions)).find(isProgramBlocked).get
    }
    val duet = Iterator.iterate(
      (ProgramState(0, Map("p" -> 0L), Queue.empty, Queue.empty, 0, isBlocked = false),
        ProgramState(0, Map("p" -> 1L), Queue.empty, Queue.empty, 0, isBlocked = false))
    ) {
      case (program0, program1) =>
        (
          runProgramUntilBlocked(program0.copy(isBlocked = false, receiveQueue = program1.sendQueue, sendQueue = Queue.empty)),
          runProgramUntilBlocked(program1.copy(isBlocked = false, receiveQueue = program0.sendQueue, sendQueue = Queue.empty))
        )

    }

    duet.drop(1).find {
      case (program0, program1) => program0.sendQueue.isEmpty && program1.sendQueue.isEmpty
    }.get._2.sendCount
  }

  def parseValue(valueStr: String): Value = {
    if (valueStr.matches("^-?[\\d]+$")) Constant(valueStr.toLong)
    else Register(valueStr)
  }

  def parseInstructionLine(line: String): Instruction = {
    line.split(" ").toList match {
      case "snd" :: register :: Nil          => Snd(register)
      case "set" :: register :: value :: Nil => Set(register, parseValue(value))
      case "add" :: register :: value :: Nil => Add(register, parseValue(value))
      case "mul" :: register :: value :: Nil => Mul(register, parseValue(value))
      case "mod" :: register :: value :: Nil => Mod(register, parseValue(value))
      case "rcv" :: register :: Nil          => Rcv(register)
      case "jgz" :: value :: offset :: Nil   => Jgz(parseValue(value), parseValue(offset))
      case _ =>
        throw new IllegalArgumentException("Unknown command line: " + line)
    }
  }

  def main(args: Array[String]): Unit = {
    val problemInput = Source
      .fromResource("day18.txt")
      .getLines
      .map(parseInstructionLine)
      .toVector
    println(solutionPart1(problemInput))
    println(solutionPart2(problemInput))
  }

}
