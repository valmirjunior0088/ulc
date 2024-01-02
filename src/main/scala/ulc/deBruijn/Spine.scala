package ulc.deBruijn

final class Spine[T] private (private val values: Vector[T]):
  def this(value: T) = this(Vector(value))
  def :+(value: T) = Spine(values :+ value)
  def apply[U](u: U, f: (U, T) => U): U = values.foldLeft(u)(f)
