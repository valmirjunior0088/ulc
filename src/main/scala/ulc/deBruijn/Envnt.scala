package ulc.deBruijn

final class Envnt[T] private (private val values: Vector[T]):
  def +:(value: T) = Envnt(value +: values)

  def indexOf(value: T) = values.indexOf(value) match
    case -1 => None
    case index => Some(Index(index))

  def apply(index: Index) =
    try Some(values(index.value))
    catch case _: IndexOutOfBoundsException => None

object Envnt:
  def initial[T] = Envnt(Vector[T]())
