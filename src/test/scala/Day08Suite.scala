import org.junit.Test
import org.junit.Assert._
import util.file
import Day08._

class Day08Suite:

  @Test def checkPosition(): Unit =
    assertEquals(position(0, -1, 2), 1)
    assertEquals(position(1, -1, 2), 0)
    assertEquals(position(1, -2, 2), 1)
    assertEquals(position(1, -3, 2), 0)
    assertEquals(position(0, -3, 2), 1)

    assertEquals(position(0, 1, 2), 1)
    assertEquals(position(0, 2, 2), 0)
    assertEquals(position(0, 3, 2), 1)
    assertEquals(position(1, 1, 2), 0)
    assertEquals(position(1, 2, 2), 1)
