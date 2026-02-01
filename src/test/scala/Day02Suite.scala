import Day02.*
import Day02Suite.*
import cats.data.NonEmptyList
import cats.parse.Parser.Error
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day02Suite extends ScalaCheckSuite:

  test("parsing the malformed 10-9 range fails"):
    assert(ProductIdRange.parser.parseAll("10-9").isLeft)

  test("parsing big input succeeds"):
    assert(parseInput(bigInput).isRight)

  test("parsing small input returns the right value"):
    assertEquals(
      parseInput(smallInput),
      NonEmptyList
        .of(
          ProductIdRange(first = ProductId(11), last = ProductId(22)),
          ProductIdRange(first = ProductId(95), last = ProductId(115)),
          ProductIdRange(first = ProductId(998), last = ProductId(1012)),
          ProductIdRange(first = ProductId(1188511880), last = ProductId(1188511890)),
          ProductIdRange(first = ProductId(222220), last = ProductId(222224)),
          ProductIdRange(first = ProductId(1698522), last = ProductId(1698528)),
          ProductIdRange(first = ProductId(446443), last = ProductId(446449)),
          ProductIdRange(first = ProductId(38593856), last = ProductId(38593862)),
          ProductIdRange(first = ProductId(565653), last = ProductId(565659)),
          ProductIdRange(first = ProductId(824824821), last = ProductId(824824827)),
          ProductIdRange(first = ProductId(2121212118), last = ProductId(2121212124))
        )
        .asRight[Error]
    )

  test("product ID 1234 is valid"):
    assert(ProductId(1234).validated.isValid)

  property("repeating twice the same sequence creates an invalid product ID"):
    forAll(Gen.choose(min = 1, max = 999)) { n =>
      assert(ProductId(s"$n$n".toLong).validated.isInvalid)
    }

  property("product IDs with an odd number of digits are valid"):
    forAll(
      Gen
        .oneOf(
          Gen.choose(min = 1L, max = 9L),
          Gen.choose(min = 100L, max = 999L),
          Gen.choose(min = 10_000L, max = 99_999L)
        )
        .map(ProductId.apply)
    )(pId => assert(pId.validated.isValid))

  test("product range converts to stream"):
    val range = ProductIdRange(first = ProductId(3), last = ProductId(5))
    assertEquals(range.toStream.toList, List(ProductId(3), ProductId(4), ProductId(5)))

  test("part 1 solution on small input is 1_227_775_554"):
    assertEquals(part1Solution(smallInput), 1_227_775_554L.asRight[Error])

  test("part 1 solution on big input is 38_310_256_125L"):
    assertEquals(part1Solution(bigInput), 38_310_256_125L.asRight[Error])

  test("product ID 1234 is (new)valid"):
    assert(ProductId(1234).newValidated.isValid)

  test("product ID 999 is NOT (new)valid"):
    assert(ProductId(999).newValidated.isInvalid)

  property("repeating at least twice the same sequence creates an invalid product ID"):
    forAll(
      Gen.choose(min = 2, max = 5),
      Gen.choose(min = 1, max = 999)
    ) { (rep, base) =>
      val pId = ProductId(List.fill(rep)(s"$base").mkString.toLong)
      assert(pId.newValidated.isInvalid)
    }

  test("part 2 solution on small input is 4_174_379_265"):
    assertEquals(part2Solution(smallInput), 4_174_379_265L.asRight[Error])

  test("part 2 solution on big input is 58_961_152_806L"):
    assertEquals(part2Solution(bigInput), 58_961_152_806L.asRight[Error])

object Day02Suite:

  val bigInput: String = getLinesFromFile("src/test/scala/day02_input.txt").head

  val smallInput: String =
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449," +
      "38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
