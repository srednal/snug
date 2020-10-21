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
  def isRoot: Boolean = false

  def isAbsolute: Boolean = parent.isAbsolute

  def asAbsolute: Path = parent.asAbsolute / name
  def asRelative: Path = parent.asRelative / name

  def elements: Seq[String] = parent.elements :+ name

  /** this / "foo" -> this/path/foo */
  def /(p: String): Path = p.split("/", 2) match {
    case Array(head, tail) => this / head / tail
    case Array("") => this
    case Array(path) => new NonEmptyPath(this, path)
  }

  /** this / that -> this/path/that/path */
  def /(p: Path): Path = p match {
    case `%` => throw new IllegalArgumentException("appending % to a path")
    case head / tail => this / head / tail
    case _ => this
  }
}

object Path {

  /** A non-empty Path. */
  private class NonEmptyPath(override val parent: Path, override val name: String) extends Path {
    require(name.nonEmpty)
    override def hashCode(): Int = parent.hashCode + name.hashCode
    override def equals(obj: Any): Boolean = obj match {
      case p: Path => p.name == name && p.parent == parent
      case _ => false
    }
    override lazy val toString = if (parent.isRoot) s"$parent$name" else s"$parent/$name"
  }

  /** Empty (root) path */
  sealed trait Root extends Path {
    override val asAbsolute = %
    override val asRelative = ^
    override val isRoot = true
    override def parent: Path = throw new NoSuchElementException(s"parent of root path ($name)")
    override val elements = Nil
    override lazy val toString = name
  }

  /** The absolute root Path.  Allows building a path from something:
    * {{{ % / "foo" / "bar" }}}
    */
  case object % extends Root {
    override val isAbsolute = true
    override val name = "/"
  }

  /** The relative root Path. Allows building a path from something:
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
    def /(p: String): Path = asPath / p
    def /(p: Path): Path = asPath / p
  }

  /** Enable {{{Path("foo/bar")}}} or {{{Path("/foo/bar")}}} */
  def apply(p: String): Path = (if (p startsWith %.name) % else ^) / p

  /** Enable {{{Path("foo", "bar", ...)}}} as a relative path */
  def apply(head: String, tail: String*): Path = tail.foldLeft(apply(head))(_ / _)

  def apply(): Path = ^

  def apply(p: Seq[String]): Path = if (p.isEmpty) ^ else Path(p.head, p.tail: _*)


  /** Extract/match as {{{foo / bar}}} */
  object / {
    def unapply(p: NonEmptyPath): Option[(Path, String)] = Option((p.parent, p.name))
  }

  /** extract/match as {{{Path(...)}}} */
  def unapplySeq(p: Path): Option[Seq[String]] = Some(p.elements)
}
