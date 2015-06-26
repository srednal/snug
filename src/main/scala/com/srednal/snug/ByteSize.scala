package com.srednal.snug

import scala.util.Try

// scalastyle:off method.name magic.number multiple.string.literals

// OK so yeah, this constructor is sorta confusing - is why it's private
// but this solves a couple of things:
//   we naturally normalize to integral bytes
//   we don't have to do a bunch of math to convert to other units
// For now (@todo: do I like this?):
// two instances are equal when both bytes and units are the same,
// so 1.KB != 1000.B even though they represent the same number of bytes
// can compare .bytes to get that case
case class ByteSize private(bytes: BigInt, units: ByteUnit) {

  lazy val sizeInUnits: BigDecimal = BigDecimal(bytes) / units.mult

  // 3-decimal precision and whack trailing zeros for neatness
  override lazy val toString = "%.3f".format(sizeInUnits).replaceAll( """\.?0+$""", "") + s" $units"

  /** 123.MB as KB => 123000.KB */
  def as(newUnits: ByteUnit): ByteSize = copy(units = newUnits)

  /** Normalize such that the size is within 1...units.base (1000 or 1024), and truncate to byte bounaraies
    * 0.5.KB.normalize => 500.B,  1234.5.MiB.normalize => 1.2345.GiB,  500.TB.normalize => 500.TB
    */
  def normalize: ByteSize = (
    // new units, one step up/down.  convert to new units via this.as, recurse until fully normalized.
    sizeInUnits match {
      case s if s >= units.base => units.next
      case s if s < 1 => units.prev
      case _ => None
    }
    ).map(as).map(_.normalize).getOrElse(this)

}

object ByteSize {

  def apply(sizeInUnits: Double, units: String): ByteSize = apply(sizeInUnits, ByteUnit(units))
  def apply(sizeInUnits: Double, units: ByteUnit): ByteSize = ByteSize((units.mult * sizeInUnits).toBigInt(), units)

  private val SizeAndUnits = """(?i)\s*(\d+\.?\d*)\s*([a-z]{0,3})\s*""".r

  def apply(s: String): ByteSize = s match {
    case SizeAndUnits(n, u) => ByteSize(n.toDouble, u)
    case _ => sys.error(s"Unable to parse ByteSize from $s")
  }

  def tryParse(s: String): Try[ByteSize] = Try(apply(s))

  def parse(s: String): Option[ByteSize] = tryParse(s).toOption

  implicit class ToByteSize[N](size: N)(implicit evidence: N => Double) {
    def B = ByteSize(size, ByteUnit.B)
    def KB = ByteSize(size, ByteUnit.KB)
    def MB = ByteSize(size, ByteUnit.MB)
    def GB = ByteSize(size, ByteUnit.GB)
    def TB = ByteSize(size, ByteUnit.TB)
    def PB = ByteSize(size, ByteUnit.PB)
    def EB = ByteSize(size, ByteUnit.EB)
    def ZB = ByteSize(size, ByteUnit.ZB)
    def YB = ByteSize(size, ByteUnit.YB)
    def KiB = ByteSize(size, ByteUnit.KiB)
    def MiB = ByteSize(size, ByteUnit.MiB)
    def GiB = ByteSize(size, ByteUnit.GiB)
    def TiB = ByteSize(size, ByteUnit.TiB)
    def PiB = ByteSize(size, ByteUnit.PiB)
    def EiB = ByteSize(size, ByteUnit.EiB)
    def ZiB = ByteSize(size, ByteUnit.ZiB)
    def YiB = ByteSize(size, ByteUnit.YiB)
  }

}

/**
 * Metric
 * 1000    kB   kilobyte
 * 1000^2  MB   megabyte
 * 1000^3  GB   gigabyte
 * 1000^4  TB   terabyte
 * 1000^5  PB   petabyte
 * 1000^6  EB   exabyte
 * 1000^7  ZB   zettabyte
 * 1000^8  YB   yottabyte
 * Binary
 * 1024    KiB  kibibyte
 * 1024^2  MiB  mebibyte
 * 1024^3  GiB  gibibyte
 * 1024^4  TiB  tebibyte
 * 1024^5  PiB  pebibyte
 * 1024^6  EiB  exbibyte
 * 1024^7  ZiB  zebibyte
 * 1024^8  YiB  yobibyte
 */
sealed abstract class ByteUnit(val base: Int, val exp: Int) {
  val mult = BigDecimal(base) pow exp
  // name relies on the instances being member objects of ByteUnit (i.e. it relies on that className format)
  val name = getClass.getSimpleName.replaceAll( """\$$""", "")
  // shortName is name without the trailing B (Ki for KiB etc)
  val shortName = name.replaceAll("[Bb]$", "")

  override val toString = name

  // used by parse to find a string match
  private def isNamed(unit: String) = unit.equalsIgnoreCase(name) || unit.equalsIgnoreCase(shortName)

  def prev: Option[ByteUnit] = if (exp == 1) Some(ByteUnit.B) else ByteUnit.units find (u => u.base == base && u.exp == exp - 1)
  def next: Option[ByteUnit] = ByteUnit.units find (u => u.base == base && u.exp == exp + 1)
}

object ByteUnit {
  def parse(s: String): Option[ByteUnit] = units find (_ isNamed s)
  def apply(s: String): ByteUnit = parse(s) getOrElse sys.error(s"Unable to parse ByteUnit from $s")

  object B extends ByteUnit(1000, 0)
  object KB extends ByteUnit(1000, 1)
  object MB extends ByteUnit(1000, 2)
  object GB extends ByteUnit(1000, 3)
  object TB extends ByteUnit(1000, 4)
  object PB extends ByteUnit(1000, 5)
  object EB extends ByteUnit(1000, 6)
  object ZB extends ByteUnit(1000, 7)
  object YB extends ByteUnit(1000, 8)
  object KiB extends ByteUnit(1024, 1)
  object MiB extends ByteUnit(1024, 2)
  object GiB extends ByteUnit(1024, 3)
  object TiB extends ByteUnit(1024, 4)
  object PiB extends ByteUnit(1024, 5)
  object EiB extends ByteUnit(1024, 6)
  object ZiB extends ByteUnit(1024, 7)
  object YiB extends ByteUnit(1024, 8)

  // all of the units above
  private val units = B :: KB :: MB :: GB :: TB :: PB :: EB :: ZB :: YB :: KiB :: MiB :: GiB :: TiB :: PiB :: EiB :: ZiB :: YiB :: Nil
}
