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
    if sub > 0 then sub else aux(sub)

  def modSubtractionWithZeroCount(lhs: Int, rhs: Int, mod: Int): (Int, Int) =
    @tailrec def aux(i: Int, acc: Int): (Int, Int) =
      if i < 0 then aux(i + mod, 1 + acc)
      else if i == 0 then (i, 1 + acc)
      else (i, acc)
    val sub = lhs - rhs
    if sub > 0 then (sub, 0) else aux(sub, acc = if lhs == 0 then -1 else 0)

  opaque type Pos = Int

  extension (p: Pos)

    def leftRotated(steps: Int): Pos = modSubtraction(lhs = p, rhs = steps, mod = Pos.dialPositions)
    def rightRotated(steps: Int): Pos = (p + steps) % Pos.dialPositions

    def rotated(r: Rotation): Pos = (r.direction match {
      case Direction.Left  => leftRotated
      case Direction.Right => rightRotated
    })(r.steps)

    def leftRotatedWithZeroCount(steps: Int): (Pos, Int) =
      modSubtractionWithZeroCount(lhs = p, rhs = steps, mod = Pos.dialPositions)
    def rightRotatedWithZeroCount(steps: Int): (Pos, Int) =
      val sum = p + steps
      (sum % Pos.dialPositions, sum / Pos.dialPositions)

    def rotatedWithZeroCount(r: Rotation): (Pos, Int) = (r.direction match {
      case Direction.Left  => leftRotatedWithZeroCount
      case Direction.Right => rightRotatedWithZeroCount
    })(r.steps)

  object Pos:
    val dialPositions: Int = 100
    val zero: Pos = 0
    val fifty: Pos = 50
    def apply(n: Int): Option[Pos] = if 0 <= n && n < dialPositions then n.some else None

  enum Direction:
    case Left, Right

  object Direction:
    def parser: Parser[Direction] = char('L').as(Left) | char('R').as(Right)

  case class Rotation(direction: Direction, steps: Int)

  object Rotation:
    def parser: Parser[Rotation] =
      (Direction.parser ~ nonNegativeIntString.mapFilter(_.toIntOption)).map(Rotation.apply)

  def parseInput(rows: List[String]): Either[Error, List[Rotation]] = rows.traverse(Rotation.parser.parseAll)

  def run(start: Pos, rotations: List[Rotation]): NonEmptyList[Pos] =
    rotations.foldLeft(NonEmptyList.one(start)) { case (acc @ NonEmptyList(p, _), r) => p.rotated(r) :: acc }.reverse

  def part1Solution(rows: List[String]): Either[Error, Long] =
    parseInput(rows).map(rotations => run(start = Pos.fifty, rotations).count(_ == Pos.zero))

  def runWithZeroCount(start: Pos, rotations: List[Rotation]): NonEmptyList[(Pos, Int)] =
    rotations
      .foldLeft(NonEmptyList.one((start, 0))) { case (acc @ NonEmptyList((p, _), _), r) =>
        p.rotatedWithZeroCount(r) :: acc
      }
      .reverse

  def part2Solution(rows: List[String]): Either[Error, Long] =
    parseInput(rows).map(rotations => runWithZeroCount(start = Pos.fifty, rotations).foldMap(_._2))
