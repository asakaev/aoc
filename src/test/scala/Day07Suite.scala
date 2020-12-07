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

  @Test def checkRule(): Unit =
    val s1 = "muted tomato bags contain 5 dim white bags, 2 vibrant gold bags, 1 vibrant coral bag."
    val s2 = "muted gold bags contain no other bags."
    val e1 = ("muted tomato",List(Bag(5,"dim white"), Bag(2,"vibrant gold"), Bag(1,"vibrant coral")))
    val e2 = ("muted gold",List())
    assertEquals(rule(s1), e1)
    assertEquals(rule(s2), e2)

  @Test def checkSolveB(): Unit =
    val xs = file.readAll("data/7f.txt")
    val r = solveB(xs)
    assertEquals(r, 126)
