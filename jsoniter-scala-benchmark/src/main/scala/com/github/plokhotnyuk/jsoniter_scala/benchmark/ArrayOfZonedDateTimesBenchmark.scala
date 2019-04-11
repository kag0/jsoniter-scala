package com.github.plokhotnyuk.jsoniter_scala.benchmark

import java.nio.charset.StandardCharsets.UTF_8
import java.time.{ZoneId, _}

import com.avsystem.commons.serialization.json._
import com.github.plokhotnyuk.jsoniter_scala.benchmark.AVSystemCodecs._
import com.github.plokhotnyuk.jsoniter_scala.benchmark.CirceEncodersDecoders._
//import com.github.plokhotnyuk.jsoniter_scala.benchmark.DslPlatformJson._
import com.github.plokhotnyuk.jsoniter_scala.benchmark.JacksonSerDesers._
import com.github.plokhotnyuk.jsoniter_scala.benchmark.JsoniterScalaCodecs._
import com.github.plokhotnyuk.jsoniter_scala.benchmark.SprayFormats._
import com.github.plokhotnyuk.jsoniter_scala.benchmark.UPickleReaderWriters._
import com.github.plokhotnyuk.jsoniter_scala.core._
import io.circe.parser._
import io.circe.syntax._
import org.openjdk.jmh.annotations.{Benchmark, Param, Setup}
import play.api.libs.json.Json
import spray.json._

import scala.collection.JavaConverters._

class ArrayOfZonedDateTimesBenchmark extends CommonParams {
  val zoneIds: Array[ZoneId] = (ZoneId.getAvailableZoneIds.asScala.take(100).map(ZoneId.of) ++
    (1 to 7).map(i => ZoneId.of(s"+0$i:00")) ++
    (1 to 7).map(i => ZoneId.of(s"UT+0$i:00")) ++
    (1 to 7).map(i => ZoneId.of(s"UTC+0$i:00")) ++
    (1 to 7).map(i => ZoneId.of(s"GMT+0$i:00"))).toArray
  @Param(Array("1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = 1000
  var obj: Array[ZonedDateTime] = _
  var jsonString: String = _
  var jsonBytes: Array[Byte] = _
  var preallocatedBuf: Array[Byte] = _

  @Setup
  def setup(): Unit = {
    obj = (1 to size).map { i =>
      val n = Math.abs(i * 1498724053)
      ZonedDateTime.of(LocalDateTime.of(LocalDate.ofEpochDay(i),
        LocalTime.ofNanoOfDay(((n % 86000) | 0x1) * 1000000000L + (i % 4 match {
          case 0 => 0
          case 1 => ((n % 1000) | 0x1) * 1000000
          case 2 => ((n % 1000000) | 0x1) * 1000
          case 3 => (n | 0x1) % 1000000000
        }))),
        zoneIds(i % zoneIds.length))
    }.toArray
    jsonString = obj.mkString("[\"", "\",\"", "\"]")
    jsonBytes = jsonString.getBytes(UTF_8)
    preallocatedBuf = new Array[Byte](jsonBytes.length + 100/*to avoid possible out of bounds error*/)
  }

  @Benchmark
  def readAVSystemGenCodec(): Array[ZonedDateTime] = JsonStringInput.read[Array[ZonedDateTime]](new String(jsonBytes, UTF_8))

  @Benchmark
  def readCirce(): Array[ZonedDateTime] = decode[Array[ZonedDateTime]](new String(jsonBytes, UTF_8)).fold(throw _, identity)
/* FIXME: DSL-JSON does not parse preferred timezone
  @Benchmark
  def readDslJsonScala(): Array[ZonedDateTime] = dslJsonDecode[Array[ZonedDateTime]](jsonBytes)
*/
  @Benchmark
  def readJacksonScala(): Array[ZonedDateTime] = jacksonMapper.readValue[Array[ZonedDateTime]](jsonBytes)

  @Benchmark
  def readJsoniterScala(): Array[ZonedDateTime] = readFromArray[Array[ZonedDateTime]](jsonBytes)

  @Benchmark
  def readPlayJson(): Array[ZonedDateTime] = Json.parse(jsonBytes).as[Array[ZonedDateTime]]

  @Benchmark
  def readSprayJson(): Array[ZonedDateTime] = JsonParser(jsonBytes).convertTo[Array[ZonedDateTime]]

  @Benchmark
  def readUPickle(): Array[ZonedDateTime] = read[Array[ZonedDateTime]](jsonBytes)

  @Benchmark
  def writeAVSystemGenCodec(): Array[Byte] = JsonStringOutput.write(obj).getBytes(UTF_8)

  @Benchmark
  def writeCirce(): Array[Byte] = printer.pretty(obj.asJson).getBytes(UTF_8)
/* FIXME: DSL-JSON does not serialize preferred timezone
  @Benchmark
  def writeDslJsonScala(): Array[Byte] = dslJsonEncode(obj)
*/
  @Benchmark
  def writeJacksonScala(): Array[Byte] = jacksonMapper.writeValueAsBytes(obj)

  @Benchmark
  def writeJsoniterScala(): Array[Byte] = writeToArray(obj)

  @Benchmark
  def writeJsoniterScalaPrealloc(): Int = writeToSubArray(obj, preallocatedBuf, 0, preallocatedBuf.length)

  @Benchmark
  def writePlayJson(): Array[Byte] = Json.toBytes(Json.toJson(obj))

  @Benchmark
  def writeSprayJson(): Array[Byte] = obj.toJson.compactPrint.getBytes(UTF_8)

  @Benchmark
  def writeUPickle(): Array[Byte] = write(obj).getBytes(UTF_8)
}