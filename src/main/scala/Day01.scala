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
    def left(n: Int): Pos = modSubtraction(lhs = p, rhs = n, mod = 100)
    def right(n: Int): Pos = (p + n) % 100

  object Pos:
    def apply(n: Int): Pos = n
    def zero: Pos = 0

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
      .foldLeft(NonEmptyList.one(start)) { case (acc @ NonEmptyList(p, tail), r) =>
        (r.d match {
          case Direction.Left  => p.left(r.n)
          case Direction.Right => p.right(r.n)
        }) :: acc
      }
      .reverse

  def part1Solution(rows: List[String]): Either[Error, Long] =
    parseInput(rows).map(rotations => run(start = Pos(50), rotations).count(_ == Pos.zero))
