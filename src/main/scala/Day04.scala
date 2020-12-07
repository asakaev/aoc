import scala.annotation.tailrec
import util.file

object Day04:

  enum Tag {
    case BirthYear, IssueYear, ExpirationYear, Height, HairColor, EyeColor, PassportId, CountryId
  }

  opaque type Passport = Map[Tag, String]
  object Passport:
    def apply(m: Map[Tag, String]): Passport = m

  import Tag._

  def tag(s: String): Tag =
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
      val (k, v) = kv(s)
      (tag(k), v)
    }
    xs.toMap

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
      xs.foldLeft(Map.empty[Tag, String])((acc, s) => passport(s) ++ acc )
    }

  def validPassportFields(p: Passport): Boolean =
    (p.keys.toSet - CountryId).size == 7

  object Validation:
    val HexColorCode = """^#([a-fA-F0-9]{6})$""".r
    val Digits9 = """^[0-9]{9}$""".r

    def validBirthYear(s: String): Boolean =
      val v = s.toInt
      1920 <= v && v <= 2002

    def validIssueYear(s: String): Boolean =
      val v = s.toInt
      2010 <= v && v <= 2020

    def validExpirationYear(s: String): Boolean =
      val v = s.toInt
      2020 <= v && v <= 2030

    def validHeight(s: String): Boolean =
      val Cm = "cm"
      val In = "in"
      s match {
        case s if s.endsWith(Cm) =>
          val v = s.split(Cm)(0).toInt
          150 <= v && v <= 193
        case s if s.endsWith(In) =>
          val v = s.split(In)(0).toInt
          59 <= v && v <= 76
        case _ => false
      }

    def validHairColor(s: String): Boolean =
      HexColorCode.matches(s)

    def validEyeColor(s: String): Boolean =
      Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(s)

    def validPasportId(s: String): Boolean =
      Digits9.matches(s)

  def validPassportData(p: Passport): Boolean =
    p.forall {
      case (BirthYear, s)      => Validation.validBirthYear(s)
      case (BirthYear, s)      => Validation.validBirthYear(s)
      case (IssueYear, s)      => Validation.validIssueYear(s)
      case (ExpirationYear, s) => Validation.validExpirationYear(s)
      case (Height, s)         => Validation.validHeight(s)
      case (HairColor, s)      => Validation.validHairColor(s)
      case (EyeColor, s)       => Validation.validEyeColor(s)
      case (PassportId, s)     => Validation.validPasportId(s)
      case (CountryId, _)      => true
    }

  def solveA(xs: List[String]): Int =
    passports(xs).count(validPassportFields)

  def solveB(xs: List[String]): Int =
    passports(xs).count(p => validPassportFields(p) && validPassportData(p))

  def main(args: Array[String]): Unit =
    val i = file.readAll("data/4a.txt")
    val r1 = solveA(i)
    val r2 = solveB(i)
    println(r1) // 245
    println(r2) // 133
