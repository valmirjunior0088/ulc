package ulc.deBruijn

final case class Level private[deBruijn] (private[deBruijn] val value: Int):
  def quote(depth: Depth) = Index(depth.value - value - 1)
