package jp.co.cyberagent.aeromock.protobuf

import jp.co.cyberagent.aeromock.AeromockProtoTypeNotFoundException
import jp.co.cyberagent.aeromock.protobuf.ProtoFieldLabel._
import jp.co.cyberagent.aeromock.protobuf.ProtoFieldType._

/**
 *
 * @author stormcat24
 */
case class ParsedProto(
  types: Map[String, List[ProtoField]],
  dependencyTypes: Map[String, List[ProtoField]]
) {

  def buildData(targetTypeName: String, data: Map[Any, Any]): ProtoProxyObject = {
    val result = types.get(targetTypeName) match {
      case None => throw new AeromockProtoTypeNotFoundException(targetTypeName)
      case Some(value) => {
        value.flatMap(f => createProtoProxyValue(data, dependencyTypes, f))
      }
    }
    ProtoProxyObject(result)
  }

  private def createProtoProxyValue(dataMap: Map[Any, Any],
      dependencies: Map[String, List[ProtoField]], f: ProtoField): Option[ProtoProxyValue[_, _]] = {

    dataMap.get(f.name) match {
      case Some(value) => {
        f match {
          case ProtoField(label, m @ MESSAGE(typeName), name, tag, _) => {
            val depFields = dependencies.get(typeName) match {
              case None => throw new RuntimeException(s"${typeName} not exist")
              case Some(value) => value
            }
            val depList = depFields.flatMap(createProtoProxyValue(value.asInstanceOf[Map[Any, Any]], dependencies, _))
            Some(ProtoProxyMessageValue(m, depList, tag))
          }
          case f @ ProtoField(REPEATED, t, _, tag, _) => Some(t.toListValue(value.asInstanceOf[List[Any]], tag))
          case f @ ProtoField(_, t, _, tag, _) => Some(t.toValue(value, tag))
        }
      }
      case None if f.label == OPTIONAL => None
      case _ => {
        throw new RuntimeException("値が指定されてない") // TODO
      }
    }
  }

}
