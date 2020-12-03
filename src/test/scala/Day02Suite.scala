import org.junit.Test
import org.junit.Assert._
import Day02._

class Day02Suite:

  @Test def checkInterval(): Unit =
    val i1 = "1-3 a: abcde"
    val i2 = "2-9 c: ccccccccc"
    val r1 = interval(i1)
    val r2 = interval(i2)
    assertEquals(r1, Interval(1, 3))
    assertEquals(r2, Interval(2, 9))

  @Test def checkSymbol(): Unit =
    val i1 = "1-3 a: abcde"
    val i2 = "2-9 c: ccccccccc"
    val r1 = symbol(i1)
    val r2 = symbol(i2)
    assertEquals(r1, 'a')
    assertEquals(r2, 'c')

  @Test def checkPassword(): Unit =
    val i1 = "1-3 a: abcde"
    val i2 = "2-9 c: ccccccccc"
    val r1 = password(i1)
    val r2 = password(i2)
    assertEquals(r1, "abcde")
    assertEquals(r2, "ccccccccc")

  @Test def checkRecord(): Unit =
    val i1 = "1-3 a: abcde"
    val i2 = "2-9 c: ccccccccc"
    val r1 = record(i1)
    val r2 = record(i2)
    assertEquals(r1, Record(Policy(Symbol('a'), Interval(1, 3)), Password("abcde")))
    assertEquals(r2, Record(Policy(Symbol('c'), Interval(2, 9)), Password("ccccccccc")))

  @Test def checkClosedInterval(): Unit =
    val i = Interval(1, 3)
    assertEquals(closedInterval(0, i), false)
    assertEquals(closedInterval(1, i), true)
    assertEquals(closedInterval(2, i), true)
    assertEquals(closedInterval(3, i), true)
    assertEquals(closedInterval(4, i), false)

  @Test def checkValidPassword(): Unit =
    val i1 = Record(Policy(Symbol('a'), Interval(1, 3)), Password("abcde"))
    val i2 = Record(Policy(Symbol('b'), Interval(1, 3)), Password("cdefg"))
    val i3 = Record(Policy(Symbol('c'), Interval(2, 9)), Password("ccccccccc"))
    assertEquals(validPassword(i1), true)
    assertEquals(validPassword(i2), false)
    assertEquals(validPassword(i3), true)

  @Test def checkValidPassword2(): Unit =
    val i1 = Record(Policy(Symbol('a'), Interval(1, 3)), Password("abcde"))
    val i2 = Record(Policy(Symbol('b'), Interval(1, 3)), Password("cdefg"))
    val i3 = Record(Policy(Symbol('c'), Interval(2, 9)), Password("ccccccccc"))
    assertEquals(validPassword2(i1), true)
    assertEquals(validPassword2(i2), false)
    assertEquals(validPassword2(i3), false)
