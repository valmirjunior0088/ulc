package ulc.deBruijn

final case class Depth private (private[deBruijn] val value: Int):
  def level = Level(value)
  def descend = Depth(value + 1)

object Depth:
  def initial = Depth(0)
