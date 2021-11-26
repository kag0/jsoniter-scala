package com.github.plokhotnyuk.jsoniter_scala.benchmark

import io.github.kag0.ninny._
import io.github.kag0.ninny.ast.JsonObject
import scala.util.Failure
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import io.github.kag0.ninny.ast.JsonValue
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter
import io.github.kag0.ninny.ast._


object NinnyFormats {
  implicit val jsoniterWritingCodec = new JsonValueCodec[JsonValue] {

    def decodeValue(in: JsonReader, default: JsonValue) = ???

    def encodeValue(x: JsonValue, out: JsonWriter): Unit = x match {
        case JsonObject(values) => 
            out.writeObjectStart()
            values.foreach{case (k, v) => 
                out.writeKey(k)
                encodeValue(v, out)
            }
            out.writeObjectEnd()
        case JsonDecimal(preciseValue) => out.writeVal(preciseValue)
        case JsonDouble(value) => out.writeVal(value)
        case JsonBlob(value) => out.writeBase64UrlVal(value.unsafeArray.asInstanceOf[Array[Byte]], false)
        case JsonString(value) => out.writeVal(value)
        case JsonFalse => out.writeVal(false)
        case JsonTrue => out.writeVal(true)
        case JsonArray(values) => 
          out.writeArrayStart()
          values.foreach(encodeValue(_, out))
          out.writeArrayEnd()
        case JsonNull => out.writeNull()
    }

    def nullValue = JsonNull
  }

  lazy implicit val adtBaseToJson: ToSomeJson[ADTBase] = {
      case x: X => adtXToJson.toSome(x) + ("type" -> "X")
      case y: Y => adtYToJson.toSome(y) + ("type" -> "Y")
      case z: Z => adtZToJson.toSome(z) + ("type" -> "Z")
  }

  implicit val adtXToJson = ToJson.auto[X]
  implicit val adtYToJson = ToJson.auto[Y]
  implicit val adtZToJson = ToJson.auto[Z]
  
  lazy implicit val adtBaseFromJson = FromJson.fromSome(json => 
    (json / "type").to[String].flatMap {
      case "X" => adtXFromJson.from(json)
      case "Y" => adtYFromJson.from(json)
      case "Z" => adtZFromJson.from(json)
    }
  )

  implicit val adtXFromJson: FromJson[X] = FromJson.auto[X]
  implicit val adtYFromJson: FromJson[Y] = FromJson.auto[Y]
  lazy implicit val adtZFromJson: FromJson[Z] = FromJson.auto[Z]

}
