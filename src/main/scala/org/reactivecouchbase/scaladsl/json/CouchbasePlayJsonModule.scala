package org.reactivecouchbase.scaladsl.json

import com.couchbase.client.deps.com.fasterxml.jackson.core.{JsonGenerator, JsonParser, JsonTokenId, Version}
import com.couchbase.client.deps.com.fasterxml.jackson.databind._
import com.couchbase.client.deps.com.fasterxml.jackson.databind.`type`.TypeFactory
import com.couchbase.client.deps.com.fasterxml.jackson.databind.deser.Deserializers
import com.couchbase.client.deps.com.fasterxml.jackson.databind.module.SimpleModule
import com.couchbase.client.deps.com.fasterxml.jackson.databind.ser.Serializers
import play.api.libs.json._

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.ListBuffer

/**
  * This file was fully copied from play-json, because of different jackson namespaces, used by the couchbase-client
  */
object CouchbasePlayJsonModule extends SimpleModule("CouchbasePlayJson", Version.unknownVersion()) {
  override def setupModule(context: Module.SetupContext): Unit = {
    context.addSerializers(new PlaySerializers)
    context.addDeserializers(new PlayDeserializers)
  }
}

class PlayDeserializers extends Deserializers.Base {
  val cls = Class.forName("play.api.libs.json.jackson.JsValueDeserializer")
  override def findBeanDeserializer(javaType: JavaType, config: DeserializationConfig, beanDesc: BeanDescription) = {
    val klass = javaType.getRawClass
    if (classOf[JsValue].isAssignableFrom(klass) || klass == JsNull.getClass) {
      new JsValueDeserializer(config.getTypeFactory, klass)
    } else null
  }

}

class PlaySerializers extends Serializers.Base {
  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription) = {
    val ser: Object = if (classOf[JsValue].isAssignableFrom(beanDesc.getBeanClass)) {
      JsValueSerializer
    } else {
      null
    }
    ser.asInstanceOf[JsonSerializer[Object]]
  }
}

object JsValueSerializer extends JsonSerializer[JsValue] {
  import java.math.{BigInteger, BigDecimal ⇒ JBigDec}

  import com.couchbase.client.deps.com.fasterxml.jackson.databind.node.{BigIntegerNode, DecimalNode}

  override def serialize(value: JsValue, json: JsonGenerator, provider: SerializerProvider) {
    value match {
      case JsNumber(v) => {
        // Workaround #3784: Same behaviour as if JsonGenerator were
        // configured with WRITE_BIGDECIMAL_AS_PLAIN, but forced as this
        // configuration is ignored when called from ObjectMapper.valueToTree
        val raw = v.bigDecimal.stripTrailingZeros.toPlainString

        if (raw contains ".") json.writeTree(new DecimalNode(new JBigDec(raw)))
        else json.writeTree(new BigIntegerNode(new BigInteger(raw)))
      }
      case JsString(v) => json.writeString(v)
      case JsBoolean(v) => json.writeBoolean(v)
      case JsArray(elements) => {
        json.writeStartArray()
        elements.foreach { t =>
          serialize(t, json, provider)
        }
        json.writeEndArray()
      }
      case JsObject(values) => {
        json.writeStartObject()
        values.foreach { t =>
          json.writeFieldName(t._1)
          serialize(t._2, json, provider)
        }
        json.writeEndObject()
      }
      case JsNull => json.writeNull()
    }
  }
}

sealed trait DeserializerContext {
  def addValue(value: JsValue): DeserializerContext
}

case class ReadingList(content: ListBuffer[JsValue]) extends DeserializerContext {
  override def addValue(value: JsValue): DeserializerContext = {
    ReadingList(content += value)
  }
}

// Context for reading an Object
case class KeyRead(content: ListBuffer[(String, JsValue)], fieldName: String) extends DeserializerContext {
  def addValue(value: JsValue): DeserializerContext = ReadingMap(content += (fieldName -> value))
}

// Context for reading one item of an Object (we already red fieldName)
case class ReadingMap(content: ListBuffer[(String, JsValue)]) extends DeserializerContext {

  def setField(fieldName: String) = KeyRead(content, fieldName)
  def addValue(value: JsValue): DeserializerContext = throw new Exception("Cannot add a value on an object without a key, malformed JSON object!")

}

class JsValueDeserializer(factory: TypeFactory, klass: Class[_]) extends JsonDeserializer[Object] {

  override def isCachable: Boolean = true

  override def deserialize(jp: JsonParser, ctxt: DeserializationContext): JsValue = {
    val value = deserialize(jp, ctxt, List())

    if (!klass.isAssignableFrom(value.getClass)) {
      throw ctxt.mappingException(klass)
    }
    value
  }

  @tailrec
  final def deserialize(jp: JsonParser, ctxt: DeserializationContext, parserContext: List[DeserializerContext]): JsValue = {
    if (jp.getCurrentToken == null) {
      jp.nextToken()
    }

    val (maybeValue, nextContext) = (jp.getCurrentToken.id(): @switch) match {

      case JsonTokenId.ID_NUMBER_INT | JsonTokenId.ID_NUMBER_FLOAT => (Some(JsNumber(jp.getDecimalValue)), parserContext)

      case JsonTokenId.ID_STRING => (Some(JsString(jp.getText)), parserContext)

      case JsonTokenId.ID_TRUE => (Some(JsBoolean(true)), parserContext)

      case JsonTokenId.ID_FALSE => (Some(JsBoolean(false)), parserContext)

      case JsonTokenId.ID_NULL => (Some(JsNull), parserContext)

      case JsonTokenId.ID_START_ARRAY => (None, ReadingList(ListBuffer()) +: parserContext)

      case JsonTokenId.ID_END_ARRAY => parserContext match {
        case ReadingList(content) :: stack => (Some(JsArray(content)), stack)
        case _ => throw new RuntimeException("We should have been reading list, something got wrong")
      }

      case JsonTokenId.ID_START_OBJECT => (None, ReadingMap(ListBuffer()) +: parserContext)

      case JsonTokenId.ID_FIELD_NAME => parserContext match {
        case (c: ReadingMap) :: stack => (None, c.setField(jp.getCurrentName) +: stack)
        case _ => throw new RuntimeException("We should be reading map, something got wrong")
      }

      case JsonTokenId.ID_END_OBJECT => parserContext match {
        case ReadingMap(content) :: stack => (Some(JsObject(content)), stack)
        case _ => throw new RuntimeException("We should have been reading an object, something got wrong")
      }

      case JsonTokenId.ID_NOT_AVAILABLE => throw new RuntimeException("We should have been reading an object, something got wrong")

      case JsonTokenId.ID_EMBEDDED_OBJECT => throw new RuntimeException("We should have been reading an object, something got wrong")
    }

    // Read ahead
    jp.nextToken()

    maybeValue match {
      case Some(v) if nextContext.isEmpty =>
        // done, no more tokens and got a value!
        // note: jp.getCurrentToken == null happens when using treeToValue (we're not parsing tokens)
        v

      case maybeValue =>
        val toPass = maybeValue.map { v =>
          val previous :: stack = nextContext
          (previous.addValue(v)) +: stack
        }.getOrElse(nextContext)

        deserialize(jp, ctxt, toPass)

    }

  }

  // This is used when the root object is null, ie when deserialising "null"
  override def getNullValue = JsNull
}