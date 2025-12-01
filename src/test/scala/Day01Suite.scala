import Day01.*
import Day01Suite.*
import cats.data.NonEmptyList
import cats.parse.Parser.Error
import cats.syntax.all.*
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class Day01Suite extends ScalaCheckSuite:

  test("small input parsed correctly"):
    assertEquals(
      parseInput(smallInput),
      List(
        Rotation(Direction.Left, 68),
        Rotation(Direction.Left, 30),
        Rotation(Direction.Right, 48),
        Rotation(Direction.Left, 5),
        Rotation(Direction.Right, 60),
        Rotation(Direction.Left, 55),
        Rotation(Direction.Left, 1),
        Rotation(Direction.Left, 99),
        Rotation(Direction.Right, 14),
        Rotation(Direction.Left, 82)
      ).asRight[Error]
    )

  test("big input parsed to something"):
    assert(parseInput(bigInput).isRight)

  property("modulo subtraction of positive numbers is always positive"):
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.posNum[Int])((lhs, rhs, mod) =>
      assert(modSubtraction(lhs, rhs, mod) >= 0)
    )

  test("run small input rotations"):
    assertEquals(
      parseInput(smallInput).map(rotations => run(start = Pos(50), rotations)),
      NonEmptyList
        .of(
          Pos(50),
          Pos(82),
          Pos(52),
          Pos(0),
          Pos(95),
          Pos(55),
          Pos(0),
          Pos(99),
          Pos(0),
          Pos(14),
          Pos(32)
        )
        .asRight[Error]
    )

  test("part 1 solution on small input is 3"):
    assertEquals(part1Solution(smallInput), 3L.asRight[Error])

  test("part 1 solution on big input is 1_007"):
    assertEquals(part1Solution(bigInput), 1_007L.asRight[Error])

object Day01Suite:

  val smallInput: List[String] = List("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day01_input.txt")
