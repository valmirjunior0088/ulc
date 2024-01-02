import munit.FunSuite
import ulc.Value

final class Evaluation extends FunSuite:
  test("Function values are never equal"):
    val idOne = Value.abs("x", x => x)
    val idTwo = Value.abs("x", x => x)
    assertNotEquals(idOne, idTwo)

  test("Function expressions can be equal up to alpha-renaming"):
    val idOne = Value.abs("x", x => x)
    val idTwo = Value.abs("y", y => y)
    assertEquals(idOne.quote, idTwo.quote)

  test("Evaluating `const id const` results in `id`"):
    val id = Value.abs("x", x => x)
    val const = Value.abs("x", x => Value.abs("y", _ => x))
    val apply = Value.app(Value.app(const, id), const)
    assertEquals(id.quote, apply.quote)
