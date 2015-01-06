package com.srednal.snug

// TODO ? - distinguish between a "dir" and a "file" (foo/bar/ vs foo/bar) ?

/**
 * A path DSL to construct /-delimited paths from strings separated with / (either as "foo/bar" or "foo" / "bar").
 */
// allow a reasonable dsl style
// scalastyle:off class.name method.name object.name
sealed trait Path {
  import Path._

  def parent: Path
  def name: String
  def isRoot = false

  def isAbsolute: Boolean = parent.isAbsolute

  def asAbsolute: Path = parent.asAbsolute / name
  def asRelative: Path = parent.asRelative / name

  def elements: Seq[String] = parent.elements :+ name

  /** this / "foo" -> this/path/foo */
  def /(p: String): Path = p split(Slash, 2) match {
    case Array(head, tail) => this / head / tail
    case Array("") => this
    case Array(path) => new /(this, path)
  }

  /** this / that -> this/path/that/path */
  def /(p: Path): Path = p match {
    case ^ => this
    case % => this
    case head / tail => this / head / tail
  }
}

object Path {

  val Slash = "/"

  /** A non-empty Path.
    *
    * The naming as / allows matchers via things such as:
    * {{{case path / sub / name =>}}}
    * or
    * {{{case ^ / "foo" / name =>}}}
    * etc.
    */
  case class /(override val parent: Path, override val name: String) extends Path {
    override lazy val toString = if (parent.isRoot) s"$parent$name" else s"$parent$Slash$name"
  }

  sealed trait Root extends Path {
    override val asAbsolute = %
    override val asRelative = ^
    override val isRoot = true
    override def parent = throw new NoSuchElementException(s"parent of root path ($name)")
    override val elements = Nil
    override lazy val toString = name
  }

  /** The root Path (absolute):
    *
    * {{{ % / "foo" / "bar" }}}
    */
  case object % extends Root {
    override val isAbsolute = true
    override val name = Slash
  }

  /** The root Path (relative):
    *
    * {{{ ^ / "foo" / "bar" }}}
    */
  case object ^ extends Root {
    override val isAbsolute = false
    override val name = ""
  }

  /**
   * Enable conversions as {{{"foo" / "bar"}}} and {{{"foo/bar/baz".asPath}}}
   */
  implicit class PathPimpedString(pathStr: String) {
    val asPath: Path = Path(pathStr)
    def /(p: String) = asPath / p
    def /(p: Path) = asPath / p
  }

  /** Enable {{{Path("foo/bar")}}} */
  def apply(p: String): Path = (if (p startsWith Slash) % else ^) / p

  /** Enable {{{Path("foo", "bar", ...)}}} */
  def apply(head: String, tail: String*): Path = tail.foldLeft(apply(head))(_ / _)

  def apply(p: Seq[String]): Path = if (p.isEmpty) ^ else Path(p.head, p.tail: _*)

  /** extract/match  as Path(...) */
  def unapplySeq(p: Path): Option[Seq[String]] = Some(p.elements)
}
