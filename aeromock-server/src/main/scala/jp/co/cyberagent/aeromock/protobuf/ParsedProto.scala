package jp.co.cyberagent.aeromock.protobuf

import jp.co.cyberagent.aeromock.AeromockProtoTypeNotFoundException

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
        value.map(f => createProtoProxyValue(data, dependencyTypes, f))
      }
    }
    ProtoProxyObject(result)
  }

  private def createProtoProxyValue(dataMap: Map[Any, Any],
      dependencies: Map[String, List[ProtoField]], f: ProtoField): ProtoProxyValue[_, _] = {

    import jp.co.cyberagent.aeromock.protobuf.ProtoFieldLabel._
    import jp.co.cyberagent.aeromock.protobuf.ProtoFieldType._

    // TODO optionalの場合の制御
    val value = dataMap.get(f.name) match {
      case Some(v) => v
      case None if f.label == OPTIONAL => f.defaultValue.get.right.get
      case _ => {
        throw new RuntimeException("値が指定されてない") // TODO
      }
    }

    f match {
      case ProtoField(label, m @ MESSAGE(typeName), name, tag, _) => {
        val depFields = dependencies.get(typeName) match {
          case None => throw new RuntimeException(s"${typeName} not exist")
          case Some(value) => value
        }
        val depList = depFields.map(createProtoProxyValue(value.asInstanceOf[Map[Any, Any]], dependencies, _))
        ProtoProxyMessageValue(m, depList, tag)
      }
      case f @ ProtoField(REPEATED, t, _, tag, _) => t.toListValue(value.asInstanceOf[List[Any]], tag)
      case f @ ProtoField(_, t, _, tag, _) => {
        println(value)
        t.toValue(value, tag)
      }
    }
  }

}
