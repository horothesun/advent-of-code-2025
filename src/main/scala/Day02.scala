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

object Day02:

  extension (n: Int) def isOdd: Boolean = n % 2 == 1

  case class ProductId(value: Long):
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

  case class ProductIdRange(first: ProductId, last: ProductId)

  object ProductIdRange:
    def parser: Parser[ProductIdRange] =
      ((ProductId.parser <* char('-')) ~ ProductId.parser).flatMap { (first, last) =>
        if first > last then failWith(s"Malformed range ${first.value}-${last.value}")
        else Parser.pure(ProductIdRange(first, last))
      }

  def parseInput(s: String): Either[Error, NonEmptyList[ProductIdRange]] =
    ProductIdRange.parser.repSep(char(',')).parseAll(s)
