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
        ProtoField(value, bitField)
      }
    }

    (t.getFullyQualifiedName -> fields)
  }
}

case class ProtoField(
  field: MessageType.Field,
  bitField: Int
)

case class ParsedRootProto (
  types: Map[String, List[ProtoField]],
  dependencyTypes: Map[String, List[ProtoField]]
)
