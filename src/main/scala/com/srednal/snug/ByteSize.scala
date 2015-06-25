package com.srednal.snug

import scala.util.Try

// scalastyle:off method.name magic.number multiple.string.literals
case class ByteSize(size: Double, units: ByteUnit) {

  lazy val bytes: BigInt = (BigDecimal(units.mult) * size).toBigInt()

  // 3-decimal precision and whack trailing zeros for neatness
  override lazy val toString = "%.3f".format(size).replaceAll( """\.?0+$""", "") + s" $units"

  /** 123.MB as KB => 123000.KB */
  def as(newUnits: ByteUnit): ByteSize = ByteSize((BigDecimal(bytes) / BigDecimal(newUnits.mult)).toDouble, newUnits)

  /** adjust to byte boundary, so that i.e. 3.14.B.truncate == 3.B and 0.1234.KB == 0.123KB */
  def trim = this as units

  /** Normalize such that the size is within 1...units.base (1000 or 1024), and truncate to byte bounaraies
    * 0.5 KB => 500 B,  1234.5 MiB => 1.2345 GiB,  500 TB => 500 TB
    */
  def normalize: ByteSize = (
    // new units, one step up/down.  convert this.as new units, re-normalize.  If not changing units, trim
    size match {
      case s if s >= units.base => units.next
      case s if size < 1 => units.prev
      case _ => None
    }
    ) map as map (_.normalize) getOrElse trim
}

object ByteSize {

  private val Patn = """(?i)\s*(\d+\.?\d*)\s*([a-z]{0,3})\s*""".r
  def apply(s: String): ByteSize = s match {
    case Patn(n, u) => ByteSize(n.toDouble, ByteUnit(u)).trim
    case _ => sys.error(s"Unable to parse ByteSize: $s")
  }

  def tryParse(s: String): Try[ByteSize] = Try(apply(s))

  def parse(s: String): Option[ByteSize] = tryParse(s).toOption

  implicit class ToByteSize[N](size: N)(implicit evidence: N => Double) {
    def B = ByteSize(size, ByteUnit.B).trim
    def KB = ByteSize(size, ByteUnit.KB).trim
    def MB = ByteSize(size, ByteUnit.MB).trim
    def GB = ByteSize(size, ByteUnit.GB).trim
    def TB = ByteSize(size, ByteUnit.TB).trim
    def PB = ByteSize(size, ByteUnit.PB).trim
    def EB = ByteSize(size, ByteUnit.EB).trim
    def ZB = ByteSize(size, ByteUnit.ZB).trim
    def YB = ByteSize(size, ByteUnit.YB).trim
    def KiB = ByteSize(size, ByteUnit.KiB).trim
    def MiB = ByteSize(size, ByteUnit.MiB).trim
    def GiB = ByteSize(size, ByteUnit.GiB).trim
    def TiB = ByteSize(size, ByteUnit.TiB).trim
    def PiB = ByteSize(size, ByteUnit.PiB).trim
    def EiB = ByteSize(size, ByteUnit.EiB).trim
    def ZiB = ByteSize(size, ByteUnit.ZiB).trim
    def YiB = ByteSize(size, ByteUnit.YiB).trim
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
  val mult = BigInt(base) pow exp
  val name = getClass.getSimpleName.replaceAll( """\$$""", "")
  val shortName = name.replaceAll("[Bb]$", "")
  def matches(unit: String) = unit.equalsIgnoreCase(name) || unit.equalsIgnoreCase(shortName)
  override val toString = name

  def prev: Option[ByteUnit] = if (exp == 1) Some(ByteUnit.B) else ByteUnit.units find (u => u.base == base && u.exp == exp - 1)
  def next: Option[ByteUnit] = ByteUnit.units find (u => u.base == base && u.exp == exp + 1)
}

object ByteUnit {
  def parse(s: String): Option[ByteUnit] = units find (_ matches s)
  def apply(s: String) = parse(s) getOrElse sys.error(s"Unable to parse ByteUnit: $s")

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

  private val units = B :: KB :: MB :: GB :: TB :: PB :: EB :: ZB :: YB :: KiB :: MiB :: GiB :: TiB :: PiB :: EiB :: ZiB :: YiB :: Nil
}
