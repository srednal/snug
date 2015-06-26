package com.srednal.snug

import scala.util.Success
import com.srednal.snug.ByteSize._
import com.srednal.snug.ByteUnit._
import org.scalatest._

// scalastyle:off magic.number multiple.string.literals
class ByteSizeTest extends WordSpec with Matchers {

  "Size" should {

    "support postfix notation" in {
      123.B shouldBe ByteSize(123, B)
      123.KB shouldBe ByteSize(123, KB)
      123.MB shouldBe ByteSize(123, MB)
      123.GB shouldBe ByteSize(123, GB)
      123.TB shouldBe ByteSize(123, TB)
      123.PB shouldBe ByteSize(123, PB)
      123.EB shouldBe ByteSize(123, EB)
      123.ZB shouldBe ByteSize(123, ZB)
      123.YB shouldBe ByteSize(123, YB)

      123.KiB shouldBe ByteSize(123, KiB)
      123.MiB shouldBe ByteSize(123, MiB)
      123.GiB shouldBe ByteSize(123, GiB)
      123.TiB shouldBe ByteSize(123, TiB)
      123.PiB shouldBe ByteSize(123, PiB)
      123.EiB shouldBe ByteSize(123, EiB)
      123.ZiB shouldBe ByteSize(123, ZiB)
      123.YiB shouldBe ByteSize(123, YiB)

      123.45.B shouldBe ByteSize(123, B)  // should trim
      123.45.KB shouldBe ByteSize(123.45, KB)
      123.45.MB shouldBe ByteSize(123.45, MB)
      123.45.GB shouldBe ByteSize(123.45, GB)
      123.45.TB shouldBe ByteSize(123.45, TB)
      123.45.PB shouldBe ByteSize(123.45, PB)
      123.45.EB shouldBe ByteSize(123.45, EB)
      123.45.ZB shouldBe ByteSize(123.45, ZB)
      123.45.YB shouldBe ByteSize(123.45, YB)

      100.5.KiB shouldBe ByteSize(100.5, KiB)
      100.5.MiB shouldBe ByteSize(100.5, MiB)
      100.5.GiB shouldBe ByteSize(100.5, GiB)
      100.5.TiB shouldBe ByteSize(100.5, TiB)
      100.5.PiB shouldBe ByteSize(100.5, PiB)
      100.5.EiB shouldBe ByteSize(100.5, EiB)
      123.5.ZiB shouldBe ByteSize(123.5, ZiB)
      123.5.YiB shouldBe ByteSize(123.5, YiB)
    }

    "convert to bytes" in {
      123.B.bytes shouldBe BigInt(123)
      123.KB.bytes shouldBe BigInt(123) * 1000
      123.MB.bytes shouldBe BigInt(123) * 1000 * 1000
      123.GB.bytes shouldBe BigInt(123) * 1000 * 1000 * 1000
      123.TB.bytes shouldBe BigInt(123) * 1000 * 1000 * 1000 * 1000
      123.PB.bytes shouldBe BigInt(123) * 1000 * 1000 * 1000 * 1000 * 1000
      123.EB.bytes shouldBe BigInt(123) * 1000 * 1000 * 1000 * 1000 * 1000 * 1000
      123.ZB.bytes shouldBe BigInt(123) * 1000 * 1000 * 1000 * 1000 * 1000 * 1000 * 1000
      123.YB.bytes shouldBe BigInt(123) * 1000 * 1000 * 1000 * 1000 * 1000 * 1000 * 1000 * 1000

      123.KiB.bytes shouldBe BigInt(123) * 1024
      123.MiB.bytes shouldBe BigInt(123) * 1024 * 1024
      123.GiB.bytes shouldBe BigInt(123) * 1024 * 1024 * 1024
      123.TiB.bytes shouldBe BigInt(123) * 1024 * 1024 * 1024 * 1024
      123.PiB.bytes shouldBe BigInt(123) * 1024 * 1024 * 1024 * 1024 * 1024
      123.EiB.bytes shouldBe BigInt(123) * 1024 * 1024 * 1024 * 1024 * 1024 * 1024
      123.ZiB.bytes shouldBe BigInt(123) * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024
      123.YiB.bytes shouldBe BigInt(123) * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024
    }

    "toString nicely" in {
      123.B.toString shouldBe "123 B"
      1234560.B.toString shouldBe "1234560 B"
      123.45.KB.toString shouldBe "123.45 KB"
      123456.MB.toString shouldBe "123456 MB"
      123.GB.toString shouldBe "123 GB"
      123.TB.toString shouldBe "123 TB"
      123456.789.PB.toString shouldBe "123456.789 PB"
      123.EB.toString shouldBe "123 EB"
      123.4.ZB.toString shouldBe "123.4 ZB"
      123.YB.toString shouldBe "123 YB"

      123.KiB.toString shouldBe "123 KiB"
      123.MiB.toString shouldBe "123 MiB"
      123.45.GiB.toString shouldBe "123.45 GiB"
      123.TiB.toString shouldBe "123 TiB"
      123.PiB.toString shouldBe "123 PiB"
      123.EiB.toString shouldBe "123 EiB"
      123.4.ZiB.toString shouldBe "123.4 ZiB"
      1230.YiB.toString shouldBe "1230 YiB"
    }

    "parse from strings" in {
      ByteSize("123 B") shouldBe 123.B
      ByteSize("1234567 B") shouldBe 1234567.B
      ByteSize("123.450 KB") shouldBe 123.45.KB
      ByteSize("123456 MB") shouldBe 123456.MB
      ByteSize("123 GB") shouldBe 123.GB
      ByteSize("123 TB") shouldBe 123.TB
      ByteSize("123456.789 PB") shouldBe 123456.789.PB
      ByteSize("123 EB") shouldBe 123.EB
      ByteSize("123 ZB") shouldBe 123.ZB
      ByteSize("123 YB") shouldBe 123.YB
      ByteSize("123 KiB") shouldBe 123.KiB
      ByteSize("123 MiB") shouldBe 123.MiB
      ByteSize("123.500 GiB") shouldBe 123.5.GiB
      ByteSize("123 TiB") shouldBe 123.TiB
      ByteSize("123 PiB") shouldBe 123.PiB
      ByteSize("123 EiB") shouldBe 123.EiB
      ByteSize("123 ZiB") shouldBe 123.ZiB
      ByteSize("123 YiB") shouldBe 123.YiB

      ByteSize("0 B") shouldBe 0.B
      ByteSize("0B") shouldBe 0.B
      ByteSize("0") shouldBe 0.B

      ByteSize("123B") shouldBe 123.B
      ByteSize("123.50KiB") shouldBe 123.5.KiB
      ByteSize("123456M") shouldBe 123456.MB

      // without trailing B
      ByteSize("123") shouldBe 123.B
      ByteSize("123.450 K") shouldBe 123.45.KB
      ByteSize("123456 M") shouldBe 123456.MB
      ByteSize("123 G") shouldBe 123.GB
      ByteSize("123 T") shouldBe 123.TB
      ByteSize("123456.789 P") shouldBe 123456.789.PB
      ByteSize("123 E") shouldBe 123.EB
      ByteSize("123 Z") shouldBe 123.ZB
      ByteSize("123 Y") shouldBe 123.YB
      ByteSize("123 Ki") shouldBe 123.KiB
      ByteSize("123 Mi") shouldBe 123.MiB
      ByteSize("123.500000 Gi") shouldBe 123.5.GiB
      ByteSize("123 Ti") shouldBe 123.TiB
      ByteSize("123 Pi") shouldBe 123.PiB
      ByteSize("123 Ei") shouldBe 123.EiB
      ByteSize("123 Zi") shouldBe 123.ZiB
      ByteSize("123 Yi") shouldBe 123.YiB

      // extra spaces
      ByteSize("     123456.789      PB  ") shouldBe 123456.789.PB

      // parse
      ByteSize.parse("123.50 Gi") shouldBe Some(123.5.GiB)

      // tryParse
      ByteSize.tryParse("123.50 Gi") shouldBe Success(123.5.GiB)
    }

    "fail parsing" in {
      a[RuntimeException] should be thrownBy ByteSize("gunk")
      a[RuntimeException] should be thrownBy ByteSize("123 QB")
      a[RuntimeException] should be thrownBy ByteSize("have 123 GiB today")

      a[RuntimeException] should be thrownBy ByteSize.tryParse("gunk").get
      a[RuntimeException] should be thrownBy ByteSize.tryParse("123 QB").get
      a[RuntimeException] should be thrownBy ByteSize.tryParse("have 123 GiB today").get

      ByteSize.parse("gunk") shouldBe None
      ByteSize.parse("123 Q") shouldBe None
      ByteSize.parse("have 123 GiB today") shouldBe None
    }

    "convert to other units" in {
      123.MB as KB shouldBe 123000.KB
      123.KB as MB shouldBe 0.123.MB
      1024.B as KiB shouldBe 1.KiB
      1.KiB as B shouldBe 1024.B
    }

    "trim to byte boundary" in {
      3.14.B shouldBe 3.B
      1.23456.KB shouldBe 1.234.KB
      0.000123123.MB shouldBe 0.000123.MB
      0.000123123.KiB shouldBe 0.KiB
      0.123123.KiB shouldBe ((0.123123 * 1024).toLong.toDouble / 1024).KiB
    }

    "normalize" in {

      1024.B.normalize shouldBe 1.024.KB
      1024.KB.normalize shouldBe 1.024.MB
      0.5.KB.normalize shouldBe 500.B

      1024.KiB.normalize shouldBe 1.MiB
      0.5.KiB.normalize shouldBe 512.B

      // multiple orders of magnitude
      1020304.MB.normalize shouldBe 1.020304.TB
      0.000012.MB.normalize shouldBe 12.B
      1020304.ZB.normalize shouldBe 1020.304.YB

      5020304.MiB.normalize shouldBe 5020304.MiB.as(TiB)

      0.000012.MiB.normalize shouldBe 12.B
      1020304.ZiB.normalize shouldBe 1020304.ZiB.as(YiB)

      // things that should not change
      10.B.normalize shouldBe 10.B
      0.B.normalize shouldBe 0.B

      1.TB shouldBe 1.TB
      1.TiB shouldBe 1.TiB

      900.TiB.normalize shouldBe 900.TiB

      1024.YB.normalize shouldBe 1024.YB

      1000.KiB.normalize shouldBe 1000.KiB // not > 1024, the KiB base
    }
  }

  "ByteUnits" should {

    "next" in {
      // check that units prev/next is ok
      B.next shouldBe Some(KB)
      KB.next shouldBe Some(MB)
      YB.next shouldBe None
      KiB.next shouldBe Some(MiB)
      YiB.next shouldBe None
    }

    "prev" in {
      MB.prev shouldBe Some(KB)
      KB.prev shouldBe Some(B)
      B.prev shouldBe None
      MiB.prev shouldBe Some(KiB)
      KiB.prev shouldBe Some(B)
    }

    "parse from strings" in {
      ByteUnit("B") shouldBe B
      ByteUnit("KB") shouldBe KB
      ByteUnit("MB") shouldBe MB
      ByteUnit("GB") shouldBe GB
      ByteUnit("TB") shouldBe TB
      ByteUnit("PB") shouldBe PB
      ByteUnit("EB") shouldBe EB
      ByteUnit("ZB") shouldBe ZB
      ByteUnit("YB") shouldBe YB
      ByteUnit("KiB") shouldBe KiB
      ByteUnit("MiB") shouldBe MiB
      ByteUnit("GiB") shouldBe GiB
      ByteUnit("TiB") shouldBe TiB
      ByteUnit("PiB") shouldBe PiB
      ByteUnit("EiB") shouldBe EiB
      ByteUnit("ZiB") shouldBe ZiB
      ByteUnit("YiB") shouldBe YiB

      // without trailing B
      ByteUnit("") shouldBe B
      ByteUnit("K") shouldBe KB
      ByteUnit("M") shouldBe MB
      ByteUnit("G") shouldBe GB
      ByteUnit("T") shouldBe TB
      ByteUnit("P") shouldBe PB
      ByteUnit("E") shouldBe EB
      ByteUnit("Z") shouldBe ZB
      ByteUnit("Y") shouldBe YB
      ByteUnit("Ki") shouldBe KiB
      ByteUnit("Mi") shouldBe MiB
      ByteUnit("Gi") shouldBe GiB
      ByteUnit("Ti") shouldBe TiB
      ByteUnit("Pi") shouldBe PiB
      ByteUnit("Ei") shouldBe EiB
      ByteUnit("Zi") shouldBe ZiB
      ByteUnit("Yi") shouldBe YiB

      // parse
      ByteUnit.parse("Gi") shouldBe Some(GiB)
    }

    "fail parsing" in {
      a[RuntimeException] should be thrownBy ByteUnit("gunk")
      a[RuntimeException] should be thrownBy ByteUnit("QB")
      a[RuntimeException] should be thrownBy ByteUnit("have  GiB today")

      ByteUnit.parse("gunk") shouldBe None
      ByteUnit.parse("Q") shouldBe None
      ByteUnit.parse("have GiB today") shouldBe None
    }

  }
}
