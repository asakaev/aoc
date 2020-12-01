import org.junit.Test
import org.junit.Assert._

class Day01Suite:
  @Test def check(): Unit =
    val input = List(1721, 979, 366, 299, 675, 1456)
    val result = Day01.solveA(input)
    assertEquals(result, Some(514579))
