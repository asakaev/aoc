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

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/7a.txt")
    val r1 = solveA(i)
    println(r1) // 259
