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

  property("modulo-subtraction of positive numbers is always non-negative"):
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.posNum[Int])((lhs, rhs, mod) =>
      assert(modSubtraction(lhs, rhs, mod) >= 0)
    )

  property("modulo-subtraction of two positive numbers in modulo 100 is always >= 0 and < 100"):
    val mod = 100
    forAll(Gen.choose(0, mod - 1), Gen.choose(0, mod - 1)) { (lhs, rhs) =>
      val sub = modSubtraction(lhs, rhs, mod)
      assert(0 <= sub && sub < mod)
    }

  test("run small input rotations from position 50"):
    assertEquals(
      parseInput(smallInput).map(rotations => run(start = Pos.fifty, rotations)),
      NonEmptyList.of(50, 82, 52, 0, 95, 55, 0, 99, 0, 14, 32).traverse(Pos.apply).get.asRight[Error]
    )

  test("part 1 solution on small input is 3"):
    assertEquals(part1Solution(smallInput), 3L.asRight[Error])

  test("part 1 solution on big input is 1_007"):
    assertEquals(part1Solution(bigInput), 1_007L.asRight[Error])

object Day01Suite:

  val smallInput: List[String] = List("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

  val bigInput: List[String] = getLinesFromFile("src/test/scala/day01_input.txt")
