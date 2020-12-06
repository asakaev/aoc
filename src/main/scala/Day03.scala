import util.file

object Day03:

  enum Square {
    case Open, Tree
  }
  
  opaque type SlopeMap = Vector[Vector[Square]]
  object SlopeMap:
    def apply(xs: Vector[Vector[Square]]): SlopeMap = xs

  final case class Point(row: Int, col: Int)
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

  def point(p: Point, s: Slope): Point =
    Point(p.row + s.down, p.col + s.right)

  def locate(p: Point, sm: SlopeMap): Option[Square] =
    val rows = sm.size
    if p.row >= rows then None
    else
      val max = sm(0).size
      val col = p.col % max
      val row = p.row
      val r = sm(row)(col)
      Some(r)
  
  def traverseSlope(sm: SlopeMap, s: Slope): Trees =
    def go(p: Point, trees: Trees): Trees =
      locate(p, sm) match {
        case None => trees
        case Some(sq) =>
          val t1 = sq match {
            case Square.Open => trees
            case Square.Tree => trees + 1
          }

          val p1 = point(p, s)
          go(p1, t1)
      }
    go(Point(0, 0), 0)

  def solveA(sm: SlopeMap): Trees =
    traverseSlope(sm, Slope(3, 1))

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/3a.txt")
    val sm = slopeMap(i)
    val r1 = solveA(sm)
    println(r1) // 225
