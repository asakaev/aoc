import scala.annotation.tailrec
import util.file

object Day07:

  opaque type Label = String
  object Label:
    def apply(s: String): Label = s

  val ShinyGold: Label = "shiny gold"

  final case class Vertex(label: Label, neighbors: List[Label])

  // TODO: ugly
  def vertex(s: String): Vertex =
    val a = s.split("contain")
    val root = a(0).split(" bag")(0)
    val r0 = a.drop(1).mkString
    val r1 = r0.split(",")
    val ls = r1.toList.map(_.split(" ").drop(2).dropRight(1).mkString(" "))
    val leafs = ls.filter(_ != "other")
    Vertex(root, leafs)

  def roots(xs: List[Vertex], leaf: Label): List[Label] =
    xs.collect { case Vertex(label, ls) if ls.contains(leaf) => label }

  def solve(xs: List[Vertex]) =
    @tailrec def go(seen: Set[String], leafs: List[String]): Set[String] =
      leafs match {
        case Nil => seen
        case h :: tl =>
          val rs = roots(xs, h)
          val next = rs ++ tl
          val acc1 = rs.toSet ++ seen
          go(acc1, next)
      }
    go(Set.empty, List(ShinyGold)).size

  def solveA(xs: List[String]): Int =
    solve(xs.map(vertex))

  //

  final case class Bag(n: Int, kind: String)

  opaque type Rule = (Label, List[Bag])
  opaque type Dictionary = Map[Label, List[Bag]]

  def rule(s: String): Rule =
    val split = s.split("contain ")
    val bag = split(0).split(" bag")(0)
    val r0 = split(1)

    val bags: List[Bag] =
      if r0.startsWith("no") then Nil
      else
        val r = r0.split(", ").toList
        r.map { s =>
          val x1 = s.split("bag").take(1)(0).dropRight(1)
          val x2 = x1.split(" ")
          val n = x2(0).toInt
          val kind = x2.drop(1).mkString(" ")
          Bag(n, kind)
        }

    (bag, bags)

  def labels(b: Bag): List[Label] =
    List.fill(b.n)(b.kind)

  def traverse(m: Dictionary): Int =
    @tailrec def go(queue: List[String], acc: Int): Int =
      queue match {
        case Nil => acc
        case h :: tl =>
          val tasks = m.getOrElse(h, Nil).flatMap(labels)
          go(tasks ++ tl, acc + tasks.size)
      }
    go(List(ShinyGold), 0)

  def dictionary(xs: List[String]): Dictionary =
    xs.foldLeft(Map.empty[Label, List[Bag]])((acc, s) => acc + rule(s))

  def solveB(xs: List[String]): Int =
    traverse(dictionary(xs))

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/7a.txt")
    val r1 = solveA(i)
    val r2 = solveB(i)
    println(r1) // 259
    println(r2) // 45018
