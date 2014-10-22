package jp.co.cyberagent.aeromock

import java.io.File
import java.nio.file.Path

import com.google.protobuf.ByteString
import com.squareup.protoparser.{MessageType, ProtoSchemaParser, Type}
import jp.co.cyberagent.aeromock.helper._
import scala.collection.JavaConverters._

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

}

class AeromockProtoParser(protobufRoot: Path) {

  def parseProto(protoFile: Path): ParsedRootProto = {

    // TODO getExtendDeclarations
    val result = ProtoSchemaParser.parse(protoFile.toFile)
    val allDeps = fetchDependencies(result.getDependencies.asScala.toSet)
    val dependencyTypes = getDependencyTypes(allDeps)
    val types = result.getTypes.asScala.toList.asInstanceOf[List[MessageType]].map(fetchType).toMap
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
      result.getTypes.asScala.toList.asInstanceOf[List[MessageType]].map(fetchType).toMap
    })
      .foldLeft(Map.empty[String, List[ProtoField]])((left, right) => left ++ right)
  }

  def fetchType(t: MessageType): (String, List[ProtoField]) = {

    var bitField = 1
    val fields = t.getFields.asScala.sortBy(_.getTag).toList.zipWithIndex.map {
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

    (t.getFullyQualifiedName -> fields)
  }
}

case class ProtoField(
                       label: ProtoFieldLabel,
                       `type`: ProtoFieldType,
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

sealed abstract class ProtoFieldType[A](val `type`: String) {

  def computeSize(tag: Int, value: A): Int
}

object ProtoFieldType {

  import com.google.protobuf.CodedOutputStream._
  import com.google.protobuf.ByteString._

  case object DOUBLE extends ProtoFieldType[Double]("double") {
    override def computeSize(tag: Int, value: Double): Int = computeDoubleSize(tag, value)
  }

  case object FLOAT extends ProtoFieldType[Float]("float") {
    override def computeSize(tag: Int, value: Float): Int = computeFloatSize(tag, value)
  }

  case object INT32 extends ProtoFieldType[Int]("int32") {
    override def computeSize(tag: Int, value: Int): Int = computeInt32Size(tag, value)
  }

  case object INT64 extends ProtoFieldType[Long]("int64") {
    override def computeSize(tag: Int, value: Long): Int = computeInt64Size(tag, value)
  }

  case object UINT32 extends ProtoFieldType[Int]("uint32") {
    override def computeSize(tag: Int, value: Int): Int = computeUInt32Size(tag, value)
  }

  case object UINT64 extends ProtoFieldType[Long]("uint64") {
    override def computeSize(tag: Int, value: Long): Int = computeUInt64Size(tag, value)
  }

  case object SINT32 extends ProtoFieldType[Int]("2int32") {
    override def computeSize(tag: Int, value: Int): Int = computeSInt32Size(tag, value)
  }

  case object SINT64 extends ProtoFieldType[Long]("2int64") {
    override def computeSize(tag: Int, value: Long): Int = computeSInt64Size(tag, value)
  }

  case object FIXED32 extends ProtoFieldType[Int]("fixed32") {
    override def computeSize(tag: Int, value: Int): Int = computeFixed32Size(tag, value)
  }

  case object FIXED64 extends ProtoFieldType[Long]("fixed64") {
    override def computeSize(tag: Int, value: Long): Int = computeFixed64Size(tag, value)
  }

  case object SFIXED32 extends ProtoFieldType[Int]("sfixed32") {
    override def computeSize(tag: Int, value: Int): Int = computeSFixed32Size(tag, value)
  }

  case object SFIXED64 extends ProtoFieldType[Long]("sfixed64") {
    override def computeSize(tag: Int, value: Long): Int = computeSFixed64Size(tag, value)
  }

  case object BOOL extends ProtoFieldType[Boolean]("bool") {
    override def computeSize(tag: Int, value: Boolean): Int = computeBoolSize(tag, value)
  }

  case object STRING extends ProtoFieldType[String]("string") {
    override def computeSize(tag: Int, value: String): Int = computeBytesSize(tag, copyFromUtf8(value))

  }

  case object BYTES extends ProtoFieldType[Array[Byte]]("bytes") {
    override def computeSize(tag: Int, value: Array[Byte]): Int = computeByteArraySize(tag, value)
  }

  val seq = Seq(
    DOUBLE,
    FLOAT,
    INT32,
    INT64,
    UINT32,
    UINT64,
    SINT32,
    SINT64,
    FIXED32,
    FIXED32,
    SFIXED32,
    SFIXED64,
    BOOL,
    STRING,
    BYTES
  )

  def valueOf(value: String): ProtoFieldType = {
    val filtered = seq.filter(_ == value)
    if (filtered.isEmpty) {
      // TODO
      throw new RuntimeException("TODO")
    } else {
      filtered.head
    }
  }

}

case class ParsedRootProto(
                            types: Map[String, List[ProtoField]],
                            dependencyTypes: Map[String, List[ProtoField]]
                            )
