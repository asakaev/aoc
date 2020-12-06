import scala.annotation.tailrec
import util.file

object Day03:

  enum Square {
    case Open, Tree
  }
  
  opaque type SlopeMap = Vector[Vector[Square]]
  object SlopeMap:
    def apply(xs: Vector[Vector[Square]]): SlopeMap = xs

  final case class Pos(row: Int, col: Int)
  final case class Slope(right: Int, down: Int)

  opaque type Trees = Int

  def square(c: Char): Square =
    c match {
      case '.' => Square.Open
      case '#' => Square.Tree
    }

  def row(s: String): Vector[Square] =
    s.map(square).toVector

  def slopeMap(xs: List[String]): SlopeMap =
    xs.toVector.map(row)

  def nextPos(p: Pos, s: Slope): Pos =
    Pos(p.row + s.down, p.col + s.right)

  def locate(p: Pos, sm: SlopeMap): Option[Square] =
    val rows = sm.size
    if p.row >= rows then None
    else
      val max = sm(0).size
      val col = p.col % max
      val row = p.row
      val r = sm(row)(col)
      Some(r)
  
  def traverseSlope(sm: SlopeMap, s: Slope): Trees =
    @tailrec def go(p: Pos, t: Trees): Trees =
      locate(p, sm) match {
        case None => t
        case Some(sq) =>
          val t1 = sq match {
            case Square.Open => t
            case Square.Tree => t + 1
          }
          go(nextPos(p, s), t1)
      }
    go(Pos(0, 0), 0)

  def solveA(sm: SlopeMap): Trees =
    traverseSlope(sm, Slope(3, 1))

  def solveB(sm: SlopeMap): Int =
    val slopes = List(
      Slope(1, 1),
      Slope(3, 1),
      Slope(5, 1),
      Slope(7, 1),
      Slope(1, 2)
    )
    slopes.foldLeft(1)((acc, s) => acc * traverseSlope(sm, s))

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/3a.txt")
    val sm = slopeMap(i)
    val r1 = solveA(sm)
    val r2 = solveB(sm)
    println(r1) // 225
    println(r2) // 1115775000
