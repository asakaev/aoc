import org.junit.Test
import org.junit.Assert._
import util.file
import Day04._
import Day04.Field._

class Day04Suite:

  @Test def checkPassport(): Unit =
    val s = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    val expected = Set(EyeColor, PassportId, ExpirationYear, HairColor)
    assertEquals(passport(s), expected)

  @Test def checkPassports(): Unit =
    val xs = file.readAll("data/4e.txt")
    val expected = List(
      Set(EyeColor, CountryId, BirthYear, HairColor, PassportId, ExpirationYear, Height, IssueYear),
      Set(EyeColor, CountryId, BirthYear, HairColor, PassportId, ExpirationYear, IssueYear),
      Set(EyeColor, BirthYear, HairColor, PassportId, ExpirationYear, Height, IssueYear),
      Set(EyeColor, HairColor, PassportId, ExpirationYear, Height, IssueYear)
    )
    assertEquals(passports(xs), expected)

  @Test def checkValidPassport(): Unit =
    val p1 = Passport(Set(EyeColor, CountryId, BirthYear, HairColor, PassportId, ExpirationYear, Height, IssueYear))
    val p2 = Passport(Set(EyeColor, CountryId, BirthYear, HairColor, PassportId, ExpirationYear, IssueYear))
    val p3 = Passport(Set(EyeColor, BirthYear, HairColor, PassportId, ExpirationYear, Height, IssueYear))
    val p4 = Passport(Set(EyeColor, HairColor, PassportId, ExpirationYear, Height, IssueYear))

    assertEquals(validPassport(p1), true)
    assertEquals(validPassport(p2), false)
    assertEquals(validPassport(p3), true)
    assertEquals(validPassport(p4), false)
