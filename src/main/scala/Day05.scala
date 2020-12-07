import util.file

object Day05:

  // 128 [0, 127]
  opaque type Row = Int
  object Row:
    def apply(i: Int): Row = i

  // 8 [0, 7]
  opaque type Col = Int
  object Col:
    def apply(i: Int): Col = i

  opaque type SeatId = Int

  final case class Range(l: Int, r: Int)

  enum Half {
    case Lower, Upper
  }

  // TODO: there is no half at start
  // TODO: naming
  final case class State(r: Range, h: Half)

  def lowerHalf(r: Range): Range =
    Range(r.l, (r.l + r.r) / 2)

  def upperHalf(r: Range): Range =
    Range((r.l + r.r + 1) / 2, r.r)

  def decode(s: String): (Row, Col) =
    val rs0 = State(Range(0, 127), Half.Lower)
    val cs0 = State(Range(0, 8), Half.Lower)
    val (rs, cs) = s.foldLeft((rs0, cs0)) {
      case ((rs, cs), c) =>
        c match {
          case 'F' => (State(lowerHalf(rs.r), Half.Lower), cs)
          case 'B' => (State(upperHalf(rs.r), Half.Upper), cs)
          case 'L' => (rs, State(lowerHalf(cs.r), Half.Lower))
          case 'R' => (rs, State(upperHalf(cs.r), Half.Upper))
        }
    }

    val r = rs.h match {
      case Half.Lower => rs.r.l
      case Half.Upper => rs.r.r
    }

    // TODO: why only lower is used?
    val c = cs.h match {
      case Half.Lower => cs.r.l
      case Half.Upper => cs.r.l
    }

    (r, c)

  def seatId(rc: (Row, Col)): SeatId =
    val (r, c) = rc
    r * 8 + c

  def solveA(xs: List[String]): Int =
    xs.map(decode).map(seatId).max


  def main(args: Array[String]): Unit =
    val i = file.readAll("data/5a.txt")
    val r1 = solveA(i)
    println(r1) // 832
