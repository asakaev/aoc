package fp

trait Monoid[A]:
  extension (x: A) def combine (y: A): A
  def unit: A

given Monoid[Int] with
  extension (x: Int) def combine (y: Int): Int = x * y
  def unit: Int = 1

object Monoid:
  def apply[T](using m: Monoid[T]) = m
