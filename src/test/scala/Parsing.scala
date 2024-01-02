import munit.FunSuite
import ulc.Parse
import ulc.Value

final class Parsing extends FunSuite:
  test("Simple"):
    val expected = Value.abs("x", x => x)
    val output = Parse("x => x")
    assertEquals(expected.quote, output.right.get)

  test("Multi-character identifier"):
    val expected = Value.abs("x", x => Value.abs("y", _ => x))
    val output = Parse("first => second => first")
    assertEquals(expected.quote, output.right.get)

  test("Let-bound name"):
    val expected = Value.abs("x", x => x)
    val output = Parse("let id = x => x; id")
    assertEquals(expected.quote, output.right.get.eval.quote)

  test("Spaces and parentheses"):
    val expected = Value.abs("x", x => x)
    val output = Parse("  let a = (x => y => x); let b = x => x; ((a) b) a  ")
    assertEquals(expected.quote, output.right.get.eval.quote)
