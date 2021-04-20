import util.file
import Math.abs
import scala.annotation.tailrec

object Day08:

  case class Instruction(op: Operation, arg: Int)
  object Instruction:
    def of(s: String): Option[Instruction] =
      s.split(" ").toList match
        case a :: b :: Nil =>
          for
            op  <- Operation.of(a)
            arg <- b.toIntOption
          yield Instruction(op, arg)
        case _ => None

  enum Operation:
    case Acc, Jmp, Nop

  object Operation:
    def of(s: String): Option[Operation] =
      s match
        case "nop" => Some(Nop)
        case "acc" => Some(Acc)
        case "jmp" => Some(Jmp)
        case _     => None

  case class State(pos: Int, acc: Int)

  enum Result:
    case Loop(acc: Int)
    case Terminaion(acc: Int)

  import Operation._
  import Result._

  def position(pos: Int, offset: Int, limit: Int): Int =
    abs(pos + offset) % limit

  def eval(is: Vector[Instruction]): Result =
    def step(i: Instruction, s: State): State =
      i.op match
        case Nop => State(s.pos + 1, s.acc)
        case Acc => State(s.pos + 1, s.acc + i.arg)
        case Jmp => State(s.pos + i.arg, s.acc)

    @tailrec def go(seen: Set[Int], s: State): Result =
      if s.pos == is.size then Terminaion(s.acc)
      else if seen.contains(s.pos) then Loop(s.acc)
      else go(seen + s.pos, step(is(s.pos), s))

    go(Set.empty, State(0, 0))

  def solve1(is: Vector[Instruction]): Option[Int] =
    eval(is) match
      case Loop(acc) =>       Some(acc)
      case Terminaion(acc) => None

  def solve2(is: Vector[Instruction]): Option[Int] =
    LazyList.from(is).zipWithIndex.map { (i, idx) =>
      val flipped: Option[Vector[Instruction]] =
        i.op match
          case Acc => None
          case Nop => Some(is.updated(idx, is(idx).copy(op = Jmp)))
          case Jmp => Some(is.updated(idx, is(idx).copy(op = Nop)))

      flipped.flatMap { xs =>
        eval(xs) match
          case Loop(_) => None
          case Terminaion(acc) => Some(acc)
      }

    }.collectFirst { case Some(acc) => acc }

  def main(args: Array[String]): Unit =
    val data: List[String] = file.readAll("data/8a.txt")
    val is: Vector[Instruction] = data.map(Instruction.of).flatten.toVector
    val r1 = solve1(is)
    val r2 = solve2(is)
    println(r1) // 2034
    println(r2) // 672
