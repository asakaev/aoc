import org.junit.Test
import org.junit.Assert._
import util.file
import Day04._
import Day04.Tag._

class Day04Suite:

  @Test def checkPassport(): Unit =
    val s = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    val expected = Map(
      EyeColor -> "gry",
      PassportId -> "860033327",
      ExpirationYear -> "2020",
      HairColor -> "#fffffd"
      )
    assertEquals(passport(s), expected)

  @Test def checkPassports(): Unit =
    val xs = file.readAll("data/4e.txt")
    val expected = List(
      Map(CountryId -> "147", PassportId -> "860033327", Height -> "183cm", ExpirationYear -> "2020", IssueYear -> "2017", HairColor -> "#fffffd", BirthYear -> "1937", EyeColor -> "gry"),
      Map(CountryId -> "350", PassportId -> "028048884", ExpirationYear -> "2023", IssueYear -> "2013", HairColor -> "#cfa07d", BirthYear -> "1929", EyeColor -> "amb"),
      Map(PassportId -> "760753108", Height -> "179cm", ExpirationYear -> "2024", IssueYear -> "2013", HairColor -> "#ae17e1", BirthYear -> "1931", EyeColor -> "brn"),
      Map(PassportId -> "166559648", EyeColor -> "brn", Height -> "59in", ExpirationYear -> "2025", IssueYear -> "2011", HairColor -> "#cfa07d")
    )
    assertEquals(passports(xs), expected)

  @Test def checkValidPassportFields(): Unit =
    val p1 = Passport(Map(CountryId -> "147", PassportId -> "860033327", Height -> "183cm", ExpirationYear -> "2020", IssueYear -> "2017", HairColor -> "#fffffd", BirthYear -> "1937", EyeColor -> "gry"))
    val p2 = Passport(Map(CountryId -> "350", PassportId -> "028048884", ExpirationYear -> "2023", IssueYear -> "2013", HairColor -> "#cfa07d", BirthYear -> "1929", EyeColor -> "amb"))
    val p3 = Passport(Map(PassportId -> "760753108", Height -> "179cm", ExpirationYear -> "2024", IssueYear -> "2013", HairColor -> "#ae17e1", BirthYear -> "1931", EyeColor -> "brn"))
    val p4 = Passport(Map(PassportId -> "166559648", EyeColor -> "brn", Height -> "59in", ExpirationYear -> "2025", IssueYear -> "2011", HairColor -> "#cfa07d"))
    assertEquals(validPassportFields(p1), true)
    assertEquals(validPassportFields(p2), false)
    assertEquals(validPassportFields(p3), true)
    assertEquals(validPassportFields(p4), false)

  @Test def checkValidHeight(): Unit =
    assertEquals(Validation.validHeight("60in"), true)
    assertEquals(Validation.validHeight("190cm"), true)
    assertEquals(Validation.validHeight("190in"), false)
    assertEquals(Validation.validHeight("190"), false)

  @Test def checkValidHairColor(): Unit =
    assertEquals(Validation.validHairColor("#123abc"), true)
    assertEquals(Validation.validHairColor("#123abz"), false)
    assertEquals(Validation.validHairColor("123abc"), false)

  @Test def checkValidPasportId(): Unit =
    assertEquals(Validation.validPasportId("000000001"), true)
    assertEquals(Validation.validPasportId("0123456789"), false)
