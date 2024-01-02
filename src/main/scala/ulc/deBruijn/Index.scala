package ulc.deBruijn

final case class Index private[deBruijn] (private[deBruijn] val value: Int):
  def eval(depth: Depth) = Level(depth.value - value - 1)
