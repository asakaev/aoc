package fp

trait Semigroup[A]:
  extension (x: A) def combine (y: A): A

given intSemigroup as Semigroup[Int]:
  extension (x: Int) def combine (y: Int): Int = x * y

object Semigroup:
  def apply[T](using m: Semigroup[T]) = m
