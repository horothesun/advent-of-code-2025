import Day02.*
import Day02Suite.*
import cats.data.NonEmptyList
import cats.parse.Parser.Error
import cats.syntax.all.*
import munit.ScalaCheckSuite

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

object Day02Suite:

  val bigInput: String = getLinesFromFile("src/test/scala/day02_input.txt").head

  val smallInput: String =
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
