package amf.plugins.document.graph.parser

case class GraphPrefixes(base: String = "", prefixes: Map[String, String] = Map(), isCompact: Boolean = false) {
  def prefix(key: String): String           = prefixes(key)
  def addPrefix(key: String, value: String) = GraphPrefixes(base, prefixes + (key -> value), isCompact)
  def withBase(b: String)                   = GraphPrefixes(b, prefixes, isCompact)
  def compact()                             = GraphPrefixes(base, prefixes, isCompact = true)

  def compactToFull(s: String): String =
    prefixes.keys
      .find(p => s.startsWith(p + ":"))
      .map(p => s.replaceFirst(p + ":", prefix(p)))
      .getOrElse(s)

  def fullToCompact(s: String): String =
    prefixes
      .collectFirst {
        case (compact, full) if s.startsWith(full) => s.replaceFirst(full, compact + ":")
      }
      .getOrElse(s)
}
