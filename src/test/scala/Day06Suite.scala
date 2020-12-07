import org.junit.Test
import org.junit.Assert._
import util.file
import Day06._

class Day06Suite:

  @Test def checkSolveA(): Unit =
    val xs = file.readAll("data/6e.txt")
    val r = solveA(xs)
    assertEquals(r, 11)

  @Test def checkSolveB(): Unit =
    val xs = file.readAll("data/6e.txt")
    val r = solveB(xs)
    assertEquals(r, 6)
