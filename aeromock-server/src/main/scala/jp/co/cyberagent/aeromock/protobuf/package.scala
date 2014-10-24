package jp.co.cyberagent.aeromock

import com.google.protobuf.{ByteString, CodedOutputStream}
import com.squareup.protoparser.MessageType

import scala.language.experimental

/**
 *
 * @author stormcat24
 */
package object protobuf {

  def getStringBytes(value: String): ByteString = ByteString.copyFromUtf8(value)

  case class ProtoField(
    label: ProtoFieldLabel,
    `type`: ProtoFieldType[_],
    name: String,
    tag: Int,
    defaultValue: Option[Either[Throwable, Any]] = None
  )

  case class ParsedRootProto(
    types: Map[String, List[ProtoField]],
    dependencyTypes: Map[String, List[ProtoField]]
  )

  case class ProtoProxyObject(
    values: List[ProtoProxyValue[_, _]]
  ) {

    lazy val serializedSize: Int = values.foldLeft(0)((left, right) => left + right.serizliedSize)

    def toByteArray(): Array[Byte] = {
      val result = new Array[Byte](serializedSize)
      val output = CodedOutputStream.newInstance(result)
      values.map(_.write(output))
      output.checkNoSpaceLeft
      result
    }
  }

  trait ProtoProxyValue[A, B] {
    val field: ProtoFieldType[A]
    val value: B

    def serizliedSize: Int
    def write(output: CodedOutputStream): Unit
  }

  case class ProtoProxySingleValue[A] (
    field: ProtoFieldType[A],
    value: A,
    tag: Int
  ) extends ProtoProxyValue[A, A] {
    override def serizliedSize: Int = field.computeSize(tag, value)
    override def write(output: CodedOutputStream): Unit = field.write(output, tag, value)
  }

  case class ProtoProxyListValue[A] (
    field: ProtoFieldType[A],
    value: List[A],
    tag: Int
  ) extends ProtoProxyValue[A, List[A]] {
    override def serizliedSize: Int = {
      value.foldLeft(0)((left, right) => left + field.computeSizeNoTag(right)) + value.size
    }
    override def write(output: CodedOutputStream): Unit = value.map(field.write(output, tag, _))
  }

  case class ProtoProxyMessageValue[A] (
    field: ProtoFieldType[A],
    value: List[ProtoProxyValue[_, _]],
    tag: Int
  ) extends ProtoProxyValue[A, List[ProtoProxyValue[_, _]]] {
    override def serizliedSize: Int = {
      value.foldLeft(0)((left, right) => left + right.serizliedSize)
    }
    override def write(output: CodedOutputStream): Unit = {
      value.map(_.write(output))
    }

  }
}


