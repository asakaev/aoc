import org.junit.Test
import org.junit.Assert._
import Day03._
import Day03.Square._

class Day03Suite:

  @Test def checkRow(): Unit =
    val s = "..##......."
    val expected = Vector(Open, Open, Tree, Tree, Open, Open, Open, Open, Open, Open, Open)
    assertEquals(row(s), expected)

  @Test def checkSlopeMap(): Unit =
    val xs = List(
      "..##.......",
      "#...#...#.."
    )
    val expected = Vector(
      Vector(Open, Open, Tree, Tree, Open, Open, Open, Open, Open, Open, Open),
      Vector(Tree, Open, Open, Open, Tree, Open, Open, Open, Tree, Open, Open)
    )
    assertEquals(slopeMap(xs), expected)

  @Test def checkPoint(): Unit =
    val p = Point(0, 0)
    val s = Slope(3, 1)
    assertEquals(point(p, s), Point(1, 3))

  @Test def checkLocate(): Unit =
    val sm = SlopeMap(
      Vector(
        Vector(Open, Open, Tree, Tree, Open, Open, Open, Open, Open, Open, Open),
        Vector(Tree, Open, Open, Open, Tree, Open, Open, Open, Tree, Open, Open)
      )
    )
    val p1 = Point(1, 3)
    val p2 = Point(1, 11)
    val p3 = Point(2, 0)
    assertEquals(locate(p1, sm), Some(Open))
    assertEquals(locate(p2, sm), Some(Tree))
    assertEquals(locate(p3, sm), None)

  @Test def checkTraverseSlope(): Unit =
    val xs = List(
      "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    )
    val sm = slopeMap(xs)
    val s = Slope(3, 1)
    val trees = traverseSlope(sm, s)
    assertEquals(trees, 7)
