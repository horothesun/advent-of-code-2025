import Day02.*
import Day02Suite.*
import cats.data.NonEmptyList
import cats.parse.Parser.Error
import cats.syntax.all.*
import munit.ScalaCheckSuite

class Day02Suite extends ScalaCheckSuite:

  test("parse small input"):
    assertEquals(
      parseInput(smallInput),
      NonEmptyList
        .of(
          ProductIdRange(first = ProductId(11), last = ProductId(22)),
          ProductIdRange(first = ProductId(95), last = ProductId(115)),
          ProductIdRange(first = ProductId(998), last = ProductId(1012)),
          ProductIdRange(first = ProductId(1188511880), ProductId(1188511890)),
          ProductIdRange(first = ProductId(222220), ProductId(222224)),
          ProductIdRange(first = ProductId(1698522), ProductId(1698528)),
          ProductIdRange(first = ProductId(446443), ProductId(446449)),
          ProductIdRange(first = ProductId(38593856), ProductId(38593862)),
          ProductIdRange(first = ProductId(565653), ProductId(565659)),
          ProductIdRange(first = ProductId(824824821), ProductId(824824827)),
          ProductIdRange(first = ProductId(2121212118), ProductId(2121212124))
        )
        .asRight[Error]
    )

  test("parse big input doesn't fail"):
    assert(parseInput(bigInput).isRight)

object Day02Suite:

  val bigInput: String = getLinesFromFile("src/test/scala/day02_input.txt").head

  val smallInput: String =
    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
