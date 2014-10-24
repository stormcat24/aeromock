package jp.co.cyberagent.aeromock.protobuf

import com.google.protobuf.{ByteString, CodedOutputStream}
import jp.co.cyberagent.aeromock.helper._

import com.google.protobuf.CodedOutputStream._

/**
 *
 * @author stormcat24
 */
sealed abstract class ProtoFieldType[A] {
  val typeName: String
  val defaultValue: A
  def toDefaultValue(value: String): Either[Throwable, A]
  def computeSize(tag: Int, value: A): Int
  def computeSizeNoTag(value: A): Int
  def write(output: CodedOutputStream, tag: Int, value: A): Unit
  def writeNoTag(output: CodedOutputStream, value: A): Unit
}

object ProtoFieldType {

  case object DOUBLE extends ProtoFieldType[Double] {
    override val typeName = "double"
    override val defaultValue = 0.0
    override def computeSize(tag: Int, value: Double): Int = computeDoubleSize(tag, value)
    override def computeSizeNoTag(value: Double): Int = computeDoubleSizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Double): Unit = output.writeDouble(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Double): Unit = output.writeDoubleNoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Double] = trye(value.toDouble)
  }

  case object FLOAT extends ProtoFieldType[Float] {
    override val typeName = "float"
    override val defaultValue = 0.0f
    override def computeSize(tag: Int, value: Float): Int = computeFloatSize(tag, value)
    override def computeSizeNoTag(value: Float): Int = computeFloatSizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Float): Unit = output.writeFloat(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Float): Unit = output.writeFloatNoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Float] = trye(value.toFloat)
  }

  case object INT32 extends ProtoFieldType[Int] {
    override val typeName = "int32"
    override val defaultValue = 0
    override def computeSize(tag: Int, value: Int): Int = computeInt32Size(tag, value)
    override def computeSizeNoTag(value: Int): Int = computeInt32SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeInt32(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeInt32NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Int] = trye(value.toInt)
  }

  case object INT64 extends ProtoFieldType[Long] {
    override val typeName = "int64"
    override val defaultValue = 0L
    override def computeSize(tag: Int, value: Long): Int = computeInt64Size(tag, value)
    override def computeSizeNoTag(value: Long): Int = computeInt64SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeInt64(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeInt64NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Long] = trye(value.toLong)
  }

  case object UINT32 extends ProtoFieldType[Int] {
    override val typeName = "uint32"
    override val defaultValue = 0
    override def computeSize(tag: Int, value: Int): Int = computeUInt32Size(tag, value)
    override def computeSizeNoTag(value: Int): Int = computeUInt32SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeUInt32(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeUInt32NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Int] = trye(value.toInt)
  }

  case object UINT64 extends ProtoFieldType[Long] {
    override val typeName = "uint64"
    override val defaultValue = 0L
    override def computeSize(tag: Int, value: Long): Int = computeUInt64Size(tag, value)
    override def computeSizeNoTag(value: Long): Int = computeUInt64SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeUInt64(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeUInt64NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Long] = trye(value.toLong)
  }

  case object SINT32 extends ProtoFieldType[Int] {
    override val typeName = "sint32"
    override val defaultValue = 0
    override def computeSize(tag: Int, value: Int): Int = computeSInt32Size(tag, value)
    override def computeSizeNoTag(value: Int): Int = computeSInt32SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeSInt32(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeSInt32NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Int] = trye(value.toInt)
  }

  case object SINT64 extends ProtoFieldType[Long] {
    override val typeName = "sint64"
    override val defaultValue = 0L
    override def computeSize(tag: Int, value: Long): Int = computeSInt64Size(tag, value)
    override def computeSizeNoTag(value: Long): Int = computeSInt64SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeSInt64(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeSInt64NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Long] = trye(value.toLong)
  }

  case object FIXED32 extends ProtoFieldType[Int] {
    override val typeName = "fixed32"
    override val defaultValue = 0
    override def computeSize(tag: Int, value: Int): Int = computeFixed32Size(tag, value)
    override def computeSizeNoTag(value: Int): Int = computeFixed32SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeFixed32(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeFixed32NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Int] = trye(value.toInt)
  }

  case object FIXED64 extends ProtoFieldType[Long] {
    override val typeName = "fixed64"
    override val defaultValue = 0L
    override def computeSize(tag: Int, value: Long): Int = computeFixed64Size(tag, value)
    override def computeSizeNoTag(value: Long): Int = computeFixed64SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeFixed64(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeFixed64NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Long] = trye(value.toLong)
  }

  case object SFIXED32 extends ProtoFieldType[Int] {
    override val typeName = "sfixed32"
    override val defaultValue = 0
    override def computeSize(tag: Int, value: Int): Int = computeSFixed32Size(tag, value)
    override def computeSizeNoTag(value: Int): Int = computeSFixed32SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Int): Unit = output.writeSFixed32(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Int): Unit = output.writeSFixed32NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Int] = trye(value.toInt)
  }

  case object SFIXED64 extends ProtoFieldType[Long] {
    override val typeName = "sfixed64"
    override val defaultValue = 0L
    override def computeSize(tag: Int, value: Long): Int = computeSFixed64Size(tag, value)
    override def computeSizeNoTag(value: Long): Int = computeSFixed64SizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Long): Unit = output.writeSFixed64(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Long): Unit = output.writeSFixed64NoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Long] = trye(value.toLong)
  }

  case object BOOL extends ProtoFieldType[Boolean] {
    override val typeName = "bool"
    override val defaultValue = false
    override def computeSize(tag: Int, value: Boolean): Int = computeBoolSize(tag, value)
    override def computeSizeNoTag(value: Boolean): Int = computeBoolSizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: Boolean): Unit = output.writeBool(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: Boolean): Unit = output.writeBoolNoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, Boolean] = trye(value.toBoolean)
  }

  case object STRING extends ProtoFieldType[String] {
    override val typeName = "string"
    override val defaultValue = ""
    override def computeSize(tag: Int, value: String): Int = computeBytesSize(tag, getStringBytes(value))
    override def computeSizeNoTag(value: String): Int = computeBytesSizeNoTag(getStringBytes(value))
    override def write(output: CodedOutputStream, tag: Int, value: String): Unit = output.writeBytes(tag, getStringBytes(value))
    override def writeNoTag(output: CodedOutputStream, value: String): Unit = output.writeBytesNoTag(getStringBytes(value))
    override def toDefaultValue(value: String): Either[Throwable, String] = trye(value)
  }

  case object BYTES extends ProtoFieldType[ByteString] {
    override val typeName = "bytes"
    override val defaultValue = ByteString.EMPTY
    override def computeSize(tag: Int, value: ByteString): Int = computeBytesSize(tag, value)
    override def computeSizeNoTag(value: ByteString): Int = computeBytesSizeNoTag(value)
    override def write(output: CodedOutputStream, tag: Int, value: ByteString): Unit = output.writeBytes(tag, value)
    override def writeNoTag(output: CodedOutputStream, value: ByteString): Unit = output.writeBytesNoTag(value)
    override def toDefaultValue(value: String): Either[Throwable, ByteString] = trye(getStringBytes(value))
  }

  case class MESSAGE(typeName: String) extends ProtoFieldType[Map[Any, Any]] {
    override val defaultValue = Map.empty[Any, Any]
    override def computeSize(tag: Int, value: Map[Any, Any]): Int = ???
    override def computeSizeNoTag(value: Map[Any, Any]): Int = ???
    override def write(output: CodedOutputStream, tag: Int, value: Map[Any, Any]): Unit = ???
    override def writeNoTag(output: CodedOutputStream, value: Map[Any, Any]): Unit = ???
    override def toDefaultValue(value: String): Either[Throwable, Map[Any, Any]] = ???
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
