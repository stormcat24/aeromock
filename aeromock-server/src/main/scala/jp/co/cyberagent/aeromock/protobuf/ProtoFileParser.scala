package jp.co.cyberagent.aeromock.protobuf

import java.nio.file.Path

import com.squareup.protoparser.{ProtoFile, EnumType, MessageType, ProtoSchemaParser}
import jp.co.cyberagent.aeromock.helper._

import scala.collection.JavaConverters._
import scalaz._
import Scalaz._

/**
 *
 * @author stormcat24
 */
class ProtoFileParser(protobufRoot: Path) {

  def parseProto(protoFile: Path): ParsedProto = {

    // TODO getExtendDeclarations
    val result = ProtoSchemaParser.parse(protoFile.toFile)
    result.getExtendDeclarations
    val allDeps = fetchDependencies(result.getDependencies.asScala.toSet)
    val dependencyTypes = getDependencyTypes(allDeps)
    result.getTypes.asScala.map {
      case mt: MessageType => (mt.getName, mt, result)
    }.map(fetchType).toList.sequenceU match {
      case Success(types) => ParsedProto(types.toMap, dependencyTypes)
      case Failure(f) => {
        throw new RuntimeException("TODO")
      }
    }
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
    val map = deps.toList.map(dep => {
      val result = ProtoSchemaParser.parse((protobufRoot / dep).toFile)
      result.getTypes.asScala.map {
        case mt: MessageType => (dep, mt, result)
      }.map(fetchType).toList.sequenceU match {
        case Success(s) => s.toMap
        case Failure(f) => throw new RuntimeException("TODO") // TODO
      }
    })
      map.foldLeft(Map.empty[String, List[ProtoField]])((left, right) => left ++ right)
  }

  def fetchType(tuple: (String, MessageType, ProtoFile)): ValidationNel[String, (String, List[ProtoField])] = {
    // TODO nest対応
    val netstedTypes = tuple._2.getNestedTypes.asScala.collect {
      case et: EnumType => (et.getName, et.getValues.asScala.toList)
    }.toMap

    val token = tuple._1.split("/")
    val fqdnParts = token.slice(0, token.length -1).toList

    val otherMap = tuple._3.getTypes.asScala.map {
      case mt: MessageType => (mt.getName, mt)
    }.toMap

    val fieldsVal = tuple._2.getFields.asScala.sortBy(_.getTag).toList.map { value =>
      ProtoFieldLabel.valueOf(value.getLabel) match {
        case Failure(f) => {
          f.toList.mkString("\n").failureNel[ProtoField]
        }
        case Success(label) => {
          val fieldType = netstedTypes.get(value.getType) match {
            case Some(enumType) => {
              ProtoFieldTypes.ENUM(value.getType, enumType.map(e => (e.getName -> e.getTag)).toMap, label)
            }
            case _ => {
              otherMap.get(value.getType) match {
                case Some(t) => {
                  ProtoFieldTypes.valueOf(fqdnParts ++ List(t.getName) mkString("", ".", ""), label)
                }
                case None => ProtoFieldTypes.valueOf(value.getType, label)
              }
            }
          }
          ProtoField(fieldType, value.getName, tag = value.getTag).successNel[String]
        }
      }
    }
    fieldsVal.sequenceU match {
      case Success(fields) => {
        val fqdn = fqdnParts ++ List(tuple._2.getName) mkString("", ".", "")
        (fqdn -> fields).successNel[String]
      }
      case Failure(f) => f.failure[(String, List[ProtoField])]
    }
  }
}
