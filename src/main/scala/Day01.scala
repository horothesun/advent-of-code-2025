import cats.data.NonEmptyList
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser
import cats.parse.Parser.{char, Error}
import cats.syntax.all.*
import scala.annotation.tailrec

object Day01:

  def modSubtraction(lhs: Int, rhs: Int, mod: Int): Int =
    @tailrec def aux(i: Int): Int = if i >= 0 then i else aux(i + mod)
    val sub = lhs - rhs
    if sub >= 0 then sub else aux(sub)

  opaque type Pos = Int

  extension (p: Pos)
    def left(n: Int): Pos = modSubtraction(lhs = p, rhs = n, mod = Pos.dialPositions)
    def right(n: Int): Pos = (p + n) % Pos.dialPositions

  object Pos:
    val dialPositions: Int = 100
    val zero: Pos = 0
    val fifty: Pos = 50
    def apply(n: Int): Option[Pos] = if 0 <= n && n < dialPositions then n.some else None

  enum Direction:
    case Left, Right

  object Direction:
    def parser: Parser[Direction] = char('L').as(Left) | char('R').as(Right)

  case class Rotation(d: Direction, n: Int)

  object Rotation:
    def parser: Parser[Rotation] =
      (Direction.parser ~ nonNegativeIntString.mapFilter(_.toIntOption)).map(Rotation.apply)

  def parseInput(rows: List[String]): Either[Error, List[Rotation]] = rows.traverse(Rotation.parser.parseAll)

  def run(start: Pos, rotations: List[Rotation]): NonEmptyList[Pos] =
    rotations
      .foldLeft(NonEmptyList.one(start)) { case (acc @ NonEmptyList(p, _), r) =>
        (r.d match {
          case Direction.Left  => p.left
          case Direction.Right => p.right
        })(r.n) :: acc
      }
      .reverse

  def part1Solution(rows: List[String]): Either[Error, Long] =
    parseInput(rows).map(rotations => run(start = Pos.fifty, rotations).count(_ == Pos.zero))
