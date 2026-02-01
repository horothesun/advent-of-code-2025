import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.*
import cats.kernel.Order
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser
import cats.parse.Parser.Error
import cats.parse.Parser.char
import cats.parse.Parser.failWith
import cats.syntax.all.*
import fs2.{Pure, Stream}

object Day02:

  extension (n: Int) def isOdd: Boolean = n % 2 == 1

  case class ProductId(value: Long):

    def next: ProductId = ProductId(1 + value)

    def validated: Validated[Unit, ProductId] =
      val s = s"$value"
      if s.size.isOdd || {
          val (lhs, rhs) = s.splitAt(s.size / 2)
          lhs != rhs
        }
      then Valid(this)
      else Invalid(())

  object ProductId:
    given Order[ProductId] = Order.by(_.value)
    def parser: Parser[ProductId] = nonNegativeIntString.mapFilter(_.toLongOption.map(ProductId.apply))

  case class ProductIdRange(first: ProductId, last: ProductId):
    def toStream: Stream[Pure, ProductId] =
      Stream.unfold(first)(pId => if pId.value > last.value then None else Some((pId, pId.next)))

  object ProductIdRange:
    def parser: Parser[ProductIdRange] =
      ((ProductId.parser <* char('-')) ~ ProductId.parser).flatMap { (first, last) =>
        if first > last then failWith(s"Malformed range ${first.value}-${last.value}")
        else Parser.pure(ProductIdRange(first, last))
      }

  def parseInput(s: String): Either[Error, NonEmptyList[ProductIdRange]] =
    ProductIdRange.parser.repSep(char(',')).parseAll(s)

  def part1Solution(s: String): Either[Error, Long] = parseInput(s).map(rs =>
    Stream
      .emits(rs.toList)
      .flatMap(r => r.toStream)
      .filter(pId => pId.validated.isInvalid)
      .foldMap(pId => pId.value)
      .toList
      .headOption
      .getOrElse(0L)
  )
