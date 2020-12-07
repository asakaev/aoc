import org.junit.Test
import org.junit.Assert._
import util.file
import Day07._

class Day07Suite:

  @Test def checkRelation(): Unit =
    val xs = file.readAll("data/7e.txt")
    val r = xs.map(relation)
    val expected = List(
      Relation("light red",List("bright white", "muted yellow")),
      Relation("dark orange",List("bright white", "muted yellow")),
      Relation("bright white",List("shiny gold")),
      Relation("muted yellow",List("shiny gold", "faded blue")),
      Relation("shiny gold",List("dark olive", "vibrant plum")),
      Relation("dark olive",List("faded blue", "dotted black")),
      Relation("vibrant plum",List("faded blue", "dotted black")),
      Relation("faded blue",List()),
      Relation("dotted black",List())
    )
    assertEquals(r, expected)

  @Test def checkRoots(): Unit =
    val xs = file.readAll("data/7e.txt")
    val rs = xs.map(relation)
    val r = roots(rs, ShinyGold)
    val expected = List("bright white", "muted yellow")
    assertEquals(r, expected)

  @Test def checkSolve(): Unit =
    val xs = file.readAll("data/7e.txt")
    val rs = xs.map(relation)
    val r = solve(rs)
    assertEquals(r, 4)
