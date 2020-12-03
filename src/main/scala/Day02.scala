import util.file

object Day02:

  opaque type Symbol = Char
  object Symbol:
    def apply(value: Char): Symbol = value

  opaque type Password = String
  object Password:
    def apply(value: String): Password = value

  final case class Interval(min: Int, max: Int)
  final case class Policy(s: Symbol, i: Interval)
  final case class Record(policy: Policy, password: Password)

  def interval(s: String): Interval =
    val xs = s.split(" ").head.split("-").map(_.toInt)
    val min = xs(0)
    val max = xs(1)
    Interval(min, max)

  def symbol(s: String): Symbol =
    s.split(" ")(1)(0)

  def password(s: String): Password =
    s.split(": ")(1)

  def record(s: String): Record =
    val i = interval(s)
    val ss = symbol(s)
    val p = password(s)
    Record(Policy(ss, i), p)

  def closedInterval(v: Int, i: Interval): Boolean =
    i.min <= v && v <= i.max

  def validPasswordA(r: Record): Boolean =
    val n = r.password.count(_ == r.policy.s)
    closedInterval(n, r.policy.i)

  def validPasswordB(r: Record): Boolean =
    val v1 = r.password(r.policy.i.min - 1)
    val v2 = r.password(r.policy.i.max - 1)
    List(v1, v2).count(_ == r.policy.s) == 1

  def solveA(xs: List[Record]): Int =
    xs.count(validPasswordA)

  def solveB(xs: List[Record]): Int =
    xs.count(validPasswordB)

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/2a.txt").map(record)
    val r1 = solveA(i)
    val r2 = solveB(i)
    println(r1) // 434
    println(r2) // 509
