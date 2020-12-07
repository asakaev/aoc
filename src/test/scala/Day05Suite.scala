import org.junit.Test
import org.junit.Assert._
import Day05._

class Day05Suite:

  @Test def checkHalf(): Unit =
    assertEquals(lowerHalf(Range(0, 127)), Range(0, 63))
    assertEquals(upperHalf(Range(0, 63)), Range(32, 63))
    assertEquals(lowerHalf(Range(32, 63)), Range(32, 47))
    assertEquals(upperHalf(Range(32, 47)), Range(40, 47))
    assertEquals(upperHalf(Range(40, 47)), Range(44, 47))
    assertEquals(lowerHalf(Range(44, 47)), Range(44, 45))
    assertEquals(lowerHalf(Range(44, 45)), Range(44, 44))

  @Test def checkDecode(): Unit =
    assertEquals(decode("FBFBBFFRLR"), (44,5))
    assertEquals(decode("FFFBBBFRRR"), (14,7))
    assertEquals(decode("BBFFBBFRLL"), (102,4))

  @Test def checkSeatId(): Unit =
    assertEquals(seatId((Row(44), Col(5))), 357)
    assertEquals(seatId((Row(14), Col(7))), 119)
    assertEquals(seatId((Row(102), Col(4))), 820)
