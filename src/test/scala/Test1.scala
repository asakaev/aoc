import org.junit.Test
import org.junit.Assert._

class Test1 {
  @Test def t1(): Unit = {
    val input = List(1721, 979, 366, 299, 675, 1456)
    val result = Main.expense(input)
    assertEquals(result, 514579)
  }
}
