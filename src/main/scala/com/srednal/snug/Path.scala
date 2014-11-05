package com.srednal.snug

// TODO ? - distinguish between absolute and relative paths (/foo/bar vs foo/bar) ?
// TODO ? - distinguish between a "dir" and a "file" (foo/bar/ vs foo/bar) ?

/**
 * A path DSL to construct /-delimited paths from strings separated with / (either as "foo/bar" or "foo" / "bar").
 */
sealed trait Path {
  import Path._

  def parent: Path
  def name: String
  def isEmpty: Boolean
  def nonEmpty = !isEmpty

  def elements: Seq[String] = parent.elements :+ name

  /** this / "foo" -> this/path/foo */
  def /(p: String): Path = p split("/", 2) match {
    case Array(head, tail) => this / head / tail
    case Array("") => this
    case Array(path) => new /(this, path)
  }

  /** this / that -> this/path/that/path */
  def /(p: Path): Path = p match {
    case ^ => this
    case head / tail => this / head / tail
  }
}


object Path {

  /** A non-empty Path.
    *
    * The naming as / allows matchers via things such as:
    * {{{case path / sub / name =>}}}
    * or
    * {{{case ^ / "foo" / name =>}}}
    * etc.
    */
  case class /(override val parent: Path, override val name: String) extends Path {
    override val isEmpty = false
    override lazy val toString = if (parent.isEmpty) s"/$name" else s"$parent/$name"
  }

  /** The root Path:
    *
    * {{{ ^ / "foo" / "bar" }}}
    */
  case object ^ extends Path {
    override val isEmpty = true
    override def parent = throw new NoSuchElementException("parent of root path")
    override val name: String = "/"
    override val elements = Nil
    override val toString = name
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
  def apply(p: String): Path = ^ / p

  /** Enable {{{Path("foo", "bar", ...)}}} */
  def apply(head: String, tail: String*): Path = tail.foldLeft(^ / head)(_ / _)

  def apply(p: Seq[String]): Path = if (p.isEmpty) ^ else Path(p.head, p.tail: _*)

  /** extract/match  as Path(...) */
  def unapplySeq(p: Path): Option[Seq[String]] = Some(p.elements)
}
