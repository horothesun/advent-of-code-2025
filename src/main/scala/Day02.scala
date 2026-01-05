import cats.data.NonEmptyList
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser
import cats.parse.Parser.Error
import cats.parse.Parser.char

object Day02:

  case class ProductId(value: Long)

  object ProductId:
    def parser: Parser[ProductId] = nonNegativeIntString.mapFilter(_.toLongOption.map(ProductId.apply))

  case class ProductIdRange(first: ProductId, last: ProductId)

  object ProductIdRange:
    def parser: Parser[ProductIdRange] = ((ProductId.parser <* char('-')) ~ ProductId.parser).map(ProductIdRange.apply)

  def parseInput(s: String): Either[Error, NonEmptyList[ProductIdRange]] =
    ProductIdRange.parser.repSep(char(',')).parseAll(s)
