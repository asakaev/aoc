import scala.annotation.tailrec
import util.file

object Day04:

  enum Field {
    case BirthYear, IssueYear, ExpirationYear, Height, HairColor, EyeColor, PassportId, CountryId
  }

  opaque type Passport = Set[Field]
  object Passport:
    def apply(s: Set[Field]): Passport = s

  import Field._

  def field(s: String): Field =
    s match {
      case "byr" => BirthYear
      case "iyr" => IssueYear
      case "eyr" => ExpirationYear
      case "hgt" => Height
      case "hcl" => HairColor
      case "ecl" => EyeColor
      case "pid" => PassportId
      case "cid" => CountryId
    }

  def kv(s: String): (String, String) =
    val xs = s.split(":")
    (xs(0), xs(1))

  def passport(s: String): Passport =
    val kvs = s.split(" ")
    val xs = kvs.map { s =>
      val (k, _) = kv(s)
      field(k)
    }
    xs.toSet

  def group[A](xs: List[A])(p: A => Boolean): List[List[A]] =
    @tailrec def go(xs: List[A], s: (List[List[A]], List[A])): (List[List[A]], List[A]) =
      (xs, s) match {
        case (Nil, (acc, as)) => (as.reverse :: acc, Nil)
        case (h :: tl, (acc, as)) =>
          if p(h) then go(tl, (as.reverse :: acc, Nil))
          else go(tl, (acc, h :: as))
      }
    go(xs, (List.empty[List[A]], List.empty[A]))._1.reverse

  def chunks(xs: List[String]): List[List[String]] =
    group(xs)(_.isEmpty)

  def passports(xs: List[String]): List[Passport] = 
    chunks(xs).map { xs =>
      xs.foldLeft(Set.empty[Field])((acc, s) => passport(s) ++ acc )
    }

  def validPassport(p: Passport): Boolean =
    (p - CountryId).size == 7

  def solveA(xs: List[String]): Int =
    passports(xs).count(validPassport)

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/4a.txt")
    val r1 = solveA(i)
    println(r1) // 245
