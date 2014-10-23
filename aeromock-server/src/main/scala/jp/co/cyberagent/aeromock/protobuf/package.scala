package jp.co.cyberagent.aeromock

import java.nio.file.Path

import com.google.protobuf.{CodedOutputStream, UnknownFieldSet, ByteString}
import com.squareup.protoparser.{MessageType, ProtoSchemaParser, Type}
import jp.co.cyberagent.aeromock.helper._
import scala.collection.JavaConverters._
import scala.language.experimental

/**
 *
 * @author stormcat24
 */
package object protobuf {


  def getStringBytes(value: String): ByteString = ByteString.copyFromUtf8(value)

  implicit class FieldExternal(value: MessageType.Field) {
    lazy val tag = value.getTag
    lazy val name = value.getName
  }

  class AeromockProtoParser(protobufRoot: Path) {

    def parseProto(protoFile: Path): ParsedRootProto = {

      // TODO getExtendDeclarations
      val result = ProtoSchemaParser.parse(protoFile.toFile)
      val allDeps = fetchDependencies(result.getDependencies.asScala.toSet)
      val dependencyTypes = getDependencyTypes(allDeps)
      val types = result.getTypes.asScala.map {
        case mt: MessageType => ("root", mt) // TODO
      }.map(fetchType).toMap

      println(types)
      println(dependencyTypes)

      ParsedRootProto(types, dependencyTypes)
    }

    def fetchDependencies(deps: Set[String]): Set[String] = {
      // TODO 存在チェックすべきか？
      deps ++ deps.map(protobufRoot / _).filter(_.exists).flatMap(dep => {
        val result = ProtoSchemaParser.parse(dep.toFile)
        fetchDependencies(result.getDependencies.asScala.toSet)
      })
    }

    def getDependencyTypes(deps: Set[String]): Map[String, List[ProtoField]] = {
      deps.toList.map(dep => {
        val result = ProtoSchemaParser.parse((protobufRoot / dep).toFile)
        result.getTypes.asScala.map {
          case mt: MessageType => (dep, mt)
        }.map(fetchType).toMap
      })
        .foldLeft(Map.empty[String, List[ProtoField]])((left, right) => left ++ right)
    }

    def fetchType(tuple: (String, MessageType)): (String, List[ProtoField]) = {

      var bitField = 1
      val fields = tuple._2.getFields.asScala.sortBy(_.getTag).toList.zipWithIndex.map {
        case (value, index) => {
          bitField = if (index == 0) bitField else bitField << 1
          ProtoField(
            label = ProtoFieldLabel.valueOf(value.getLabel),
            `type` = ProtoFieldType.valueOf(value.getType),
            name = value.getName,
            tag = value.getTag,
            documentation = value.getDocumentation,
            bitField = bitField
          )
        }
      }

      val token = tuple._1.split("/")
      val fqdn = token.slice(0, token.length -1).toList ++ List(tuple._2.getName) mkString("", ".", "")
      (fqdn -> fields)
    }
  }

  case class ProtoField(
    label: ProtoFieldLabel,
    `type`: ProtoFieldType[_],
    name: String,
    tag: Int,
    // TODO OPTION
    documentation: String,
    bitField: Int
  )

  sealed abstract class ProtoFieldLabel

  object ProtoFieldLabel {

    case object REQUIRED extends ProtoFieldLabel

    case object OPTIONAL extends ProtoFieldLabel

    case object REPEATED extends ProtoFieldLabel

    val map = Map(
      MessageType.Label.REQUIRED -> REQUIRED,
      MessageType.Label.OPTIONAL -> OPTIONAL,
      MessageType.Label.REPEATED -> REPEATED
    )

    def valueOf(label: MessageType.Label): ProtoFieldLabel = {
      map.get(label) match {
        case Some(v) => v
        case _ => throw new RuntimeException("TODO") // TODO
      }
    }
  }

  sealed abstract class ProtoFieldType[A] {
    val typeName: String
    val defaultValue: A
    def computeSize(tag: Int, value: A): Int
    def computeSizeNoTag(value: A): Int
    def write(output: CodedOutputStream, tag: Int, value: A): Unit
    def writeNoTag(output: CodedOutputStream, value: A): Unit
  }

  object ProtoFieldType {

    import com.google.protobuf.CodedOutputStream._
    import com.google.protobuf.ByteString._

    case object DOUBLE extends ProtoFieldType[Double] {
      override val typeName = "double"
      override val defaultValue = 0.0
      override def computeSize(tag: Int, value: Double): Int = computeDoubleSize(tag, value)
      override def computeSizeNoTag(value: Double): Int = computeDoubleSizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Double): Unit = output.writeDouble(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Double): Unit = output.writeDoubleNoTag(value)
    }

    case object FLOAT extends ProtoFieldType[Float] {
      override val typeName = "float"
      override val defaultValue = 0.0f
      override def computeSize(tag: Int, value: Float): Int = computeFloatSize(tag, value)
      override def computeSizeNoTag(value: Float): Int = computeFloatSizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Float): Unit = output.writeFloat(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Float): Unit = output.writeFloatNoTag(value)
    }

    case object INT32 extends ProtoFieldType[Int] {
      override val typeName = "int32"
      override val defaultValue = 0
      override def computeSize(tag: Int, value: Int): Int = computeInt32Size(tag, value)
      override def computeSizeNoTag(value: Int): Int = computeInt32SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeInt32(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeInt32NoTag(value)
    }

    case object INT64 extends ProtoFieldType[Long] {
      override val typeName = "int64"
      override val defaultValue = 0L
      override def computeSize(tag: Int, value: Long): Int = computeInt64Size(tag, value)
      override def computeSizeNoTag(value: Long): Int = computeInt64SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeInt64(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeInt64NoTag(value)
    }

    case object UINT32 extends ProtoFieldType[Int] {
      override val typeName = "uint32"
      override val defaultValue = 0
      override def computeSize(tag: Int, value: Int): Int = computeUInt32Size(tag, value)
      override def computeSizeNoTag(value: Int): Int = computeUInt32SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeUInt32(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeUInt32NoTag(value)
    }

    case object UINT64 extends ProtoFieldType[Long] {
      override val typeName = "uint64"
      override val defaultValue = 0L
      override def computeSize(tag: Int, value: Long): Int = computeUInt64Size(tag, value)
      override def computeSizeNoTag(value: Long): Int = computeUInt64SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeUInt64(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeUInt64NoTag(value)
    }

    case object SINT32 extends ProtoFieldType[Int] {
      override val typeName = "sint32"
      override val defaultValue = 0
      override def computeSize(tag: Int, value: Int): Int = computeSInt32Size(tag, value)
      override def computeSizeNoTag(value: Int): Int = computeSInt32SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeSInt32(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeSInt32NoTag(value)
    }

    case object SINT64 extends ProtoFieldType[Long] {
      override val typeName = "sint64"
      override val defaultValue = 0L
      override def computeSize(tag: Int, value: Long): Int = computeSInt64Size(tag, value)
      override def computeSizeNoTag(value: Long): Int = computeSInt64SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeSInt64(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeSInt64NoTag(value)
    }

    case object FIXED32 extends ProtoFieldType[Int] {
      override val typeName = "fixed32"
      override val defaultValue = 0
      override def computeSize(tag: Int, value: Int): Int = computeFixed32Size(tag, value)
      override def computeSizeNoTag(value: Int): Int = computeFixed32SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeFixed32(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeFixed32NoTag(value)
    }

    case object FIXED64 extends ProtoFieldType[Long] {
      override val typeName = "fixed64"
      override val defaultValue = 0L
      override def computeSize(tag: Int, value: Long): Int = computeFixed64Size(tag, value)
      override def computeSizeNoTag(value: Long): Int = computeFixed64SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeFixed64(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeFixed64NoTag(value)
    }

    case object SFIXED32 extends ProtoFieldType[Int] {
      override val typeName = "sfixed32"
      override val defaultValue = 0
      override def computeSize(tag: Int, value: Int): Int = computeSFixed32Size(tag, value)
      override def computeSizeNoTag(value: Int): Int = computeSFixed32SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeSFixed32(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeSFixed32NoTag(value)
    }

    case object SFIXED64 extends ProtoFieldType[Long] {
      override val typeName = "sfixed64"
      override val defaultValue = 0L
      override def computeSize(tag: Int, value: Long): Int = computeSFixed64Size(tag, value)
      override def computeSizeNoTag(value: Long): Int = computeSFixed64SizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeSFixed64(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeSFixed64NoTag(value)
    }

    case object BOOL extends ProtoFieldType[Boolean] {
      override val typeName = "bool"
      override val defaultValue = false
      override def computeSize(tag: Int, value: Boolean): Int = computeBoolSize(tag, value)
      override def computeSizeNoTag(value: Boolean): Int = computeBoolSizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: Boolean): Unit = output.writeBool(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: Boolean): Unit = output.writeBoolNoTag(value)
    }

    case object STRING extends ProtoFieldType[String] {
      override val typeName = "string"
      override val defaultValue = ""
      override def computeSize(tag: Int, value: String): Int = computeBytesSize(tag, copyFromUtf8(value))
      override def computeSizeNoTag(value: String): Int = computeBytesSizeNoTag(copyFromUtf8(value))
      override def write(output: CodedOutputStream, tag: Int, value: String): Unit = output.writeBytes(tag, copyFromUtf8(value))
      override def writeNoTag(output: CodedOutputStream, value: String): Unit = output.writeBytesNoTag(copyFromUtf8(value))
    }

    case object BYTES extends ProtoFieldType[ByteString] {
      override val typeName = "bytes"
      override val defaultValue = ByteString.EMPTY
      override def computeSize(tag: Int, value: ByteString): Int = computeBytesSize(tag, value)
      override def computeSizeNoTag(value: ByteString): Int = computeBytesSizeNoTag(value)
      override def write(output: CodedOutputStream, tag: Int, value: ByteString): Unit = output.writeBytes(tag, value)
      override def writeNoTag(output: CodedOutputStream, value: ByteString): Unit = output.writeBytesNoTag(value)
    }

    case class MESSAGE(typeName: String) extends ProtoFieldType[Map[Any, Any]] {
      override val defaultValue = Map.empty[Any, Any]
      override def computeSize(tag: Int, value: Map[Any, Any]): Int = ???
      override def computeSizeNoTag(value: Map[Any, Any]): Int = ???
      override def write(output: CodedOutputStream, tag: Int, value: Map[Any, Any]): Unit = ???
      override def writeNoTag(output: CodedOutputStream, value: Map[Any, Any]): Unit = ???
    }

    def valueOf(value: String): ProtoFieldType[_] = {
      value match {
        case "double" => DOUBLE
        case "float" => FLOAT
        case "int32" => INT32
        case "int64" => INT64
        case "uint32" => UINT32
        case "uint64" => UINT64
        case "sint32" => SINT32
        case "sint64" => SINT64
        case "fixed32" => FIXED32
        case "fixed64" => FIXED64
        case "sfixed32" => SFIXED32
        case "sfixed64" => SFIXED64
        case "bool" => BOOL
        case "string" => STRING
        case "bytes" => BYTES
        case _ => MESSAGE(value)
      }
    }

  }

  case class ParsedRootProto(
    types: Map[String, List[ProtoField]],
    dependencyTypes: Map[String, List[ProtoField]]
  )

  case class ProtoProxyObject(
    bitField: Int,
    values: List[ProtoProxyValue[_, _]]
  ) {

    lazy val serializedSize: Int = values.foldLeft(0)((left, right) => left + right.serizliedSize)

    def toByteArray(): Array[Byte] = {
      val result = new Array[Byte](serializedSize)
      val output = CodedOutputStream.newInstance(result)
      values.map(_.write(output))
      // TODO write
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


