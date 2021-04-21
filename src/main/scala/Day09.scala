import util.file

object Day09:

  def solve1(xs: LazyList[Int], n: Int): Option[Int] =
    xs
      .scanLeft(List.empty[Int])((xs, x) => (x :: xs).take(n + 1))
      .dropWhile(_.size <= n)
      .flatMap {
        case x :: xs =>
          val exists = xs.combinations(2).exists {
            case a :: b :: Nil => a != b && a + b == x
            case _             => false
          }

          if !exists then LazyList(x)
          else LazyList.empty
        case _ => LazyList.empty
      }
      .headOption

  def solve2(xs: LazyList[BigDecimal], n: BigDecimal): Option[BigDecimal] =
    val tails: LazyList[LazyList[BigDecimal]] =
      LazyList.unfold(xs) {
        case LazyList() => None
        case _ #:: tl   => Some((tl, tl))
      }

    (LazyList(xs) ++ tails).flatMap { xs =>
      xs
        .scanLeft(BigDecimal(0), BigDecimal(Int.MaxValue), BigDecimal(0)) {
          case ((acc, mn, mx), x) => (acc + x, mn.min(x), mx.max(x))
        }
        .dropWhile((sum, _, _) => sum != n)
        .map((_, min, max) => min + max)
        .take(1)
    }
    .headOption

  def main(args: Array[String]): Unit =
    val data: LazyList[String] = file.readAllLazy("data/9a.txt")
    val xs: LazyList[Int] = data.map(_.toInt)
    val ys: LazyList[BigDecimal] = data.map(BigDecimal.apply)
    val r1 = solve1(xs, 25)
    val r2 = solve2(ys, BigDecimal(144381670))
    println(r1) // 144381670
    println(r2) // 20532569
