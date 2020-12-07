import scala.annotation.tailrec
import util.file

object Day07:

  val ShinyGold = "shiny gold"

  final case class Relation(root: String, leafs: List[String])

  // TODO: ugly
  def relation(s: String): Relation =
    val a = s.split("contain")
    val root = a(0).split(" bag")(0)
    val r0 = a.drop(1).mkString
    val r1 = r0.split(",")
    val ls = r1.toList.map(_.split(" ").drop(2).dropRight(1).mkString(" "))
    val leafs = ls.filter(_ != "other")
    Relation(root, leafs)

  def roots(xs: List[Relation], leaf: String): List[String] =
    xs.collect { case Relation(root, ls) if ls.contains(leaf)  => root }

  def solve(xs: List[Relation]) =
    @tailrec def go(acc: Set[String], leafs: List[String]): Set[String] =
      leafs match {
        case Nil => acc
        case h :: tl =>
          val rs = roots(xs, h)
          val next = rs ++ tl
          val acc1 = rs.toSet ++ acc
          go(acc1, next)
      }

    val s = go(Set.empty, List(ShinyGold))
    s.size

  def solveA(xs: List[String]): Int =
    solve(xs.map(relation))

  //

  final case class Bag(n: Int, kind: String)

  opaque type Label = String
  opaque type Neighbors = List[Bag]
  opaque type Rule = (Label, Neighbors)
  opaque type Dictionary = Map[Label, Neighbors]

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
    xs.foldLeft(Map.empty[Label, Neighbors])((acc, s) => acc + rule(s))

  def solveB(xs: List[String]): Int =
    val m = dictionary(xs)
    traverse(m)

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/7a.txt")
    val r1 = solveA(i)
    val r2 = solveB(i)
    println(r1) // 259
    println(r2) // 45018
