package jp.co.cyberagent.aeromock.protobuf

import java.nio.file.Path

import jp.co.cyberagent.aeromock.helper._
import com.squareup.protoparser.{MessageType, ProtoSchemaParser}
import scala.collection.JavaConverters._
import scalaz._
import Scalaz._

/**
 *
 * @author stormcat24
 */
class AeromockProtoParser(protobufRoot: Path) {

  def parseProto(protoFile: Path): ParsedRootProto = {

    // TODO getExtendDeclarations
    val result = ProtoSchemaParser.parse(protoFile.toFile)
    val allDeps = fetchDependencies(result.getDependencies.asScala.toSet)
    val dependencyTypes = getDependencyTypes(allDeps)
    val types = result.getTypes.asScala.map {
      case mt: MessageType => {
        val nestedTypes = mt.getNestedTypes.asScala.toList
        println(nestedTypes)
        (mt.getName, mt)
      }
    }.map(fetchType).toMap

    ParsedRootProto(types, dependencyTypes)
  }

  def fetchDependencies(deps: Set[String]): Set[String] = {
    deps ++ deps.map(protobufRoot / _).flatMap { path =>
      if (path.exists) {
        val result = ProtoSchemaParser.parse(path.toFile)
        fetchDependencies(result.getDependencies.asScala.toSet)
      } else {
        throw new RuntimeException(s"${path} is not found.") // TODO
      }
    }
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

    val fields = tuple._2.getFields.asScala.sortBy(_.getTag).toList.zipWithIndex.map {
      case (value, index) => {
        val fieldType = ProtoFieldType.valueOf(value.getType)
        val defaultValue = value.getOptions.asScala.collectFirst {
          case o if o.getName == "default" => fieldType.toDefaultValue(o.getValue.toString)
        }
        ProtoField(
          label = ProtoFieldLabel.valueOf(value.getLabel),
          `type` = fieldType,
          name = value.getName,
          tag = value.getTag,
          defaultValue
        )
      }
    }

    val token = tuple._1.split("/")
    val fqdn = token.slice(0, token.length -1).toList ++ List(tuple._2.getName) mkString("", ".", "")
    (fqdn -> fields)
  }
}
