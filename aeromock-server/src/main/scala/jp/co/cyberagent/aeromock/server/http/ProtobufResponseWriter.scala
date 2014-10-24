package jp.co.cyberagent.aeromock.server.http

import com.google.protobuf.ByteString
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{FullHttpRequest, HttpResponse, HttpResponseStatus}
import jp.co.cyberagent.aeromock._
import jp.co.cyberagent.aeromock.config.Project
import jp.co.cyberagent.aeromock.core.el.VariableHelper
import jp.co.cyberagent.aeromock.core.http.VariableManager
import jp.co.cyberagent.aeromock.helper.DeepTraversal._
import jp.co.cyberagent.aeromock.helper._
import jp.co.cyberagent.aeromock.protobuf._
import org.apache.commons.lang3.StringUtils
import org.yaml.snakeyaml.{Yaml, DumperOptions}
import scaldi.Injector

import scala.collection.JavaConverters._

/**
 * [[jp.co.cyberagent.aeromock.server.http.HttpRequestProcessor]] for Google Protocol Buffers.
 * @author stormcat24
 */
class ProtobufResponseWriter(implicit inj: Injector) extends HttpRequestProcessor
  with HttpResponseWriter with ResponseDataSupport {

  val project = inject[Project]

  /**
   *
   * @param request [[FullHttpRequest]]
   * @param context [[ChannelHandlerContext]]
   * @return [[HttpResponse]]
   */
  override def process(request: FullHttpRequest)(implicit context: ChannelHandlerContext): HttpResponse = {
    val protobuf = project._protobuf
    val naming = project._naming

    val protoFile = protobuf.apiPrefix match {
      case Some(prefix) => protobuf.root / prefix / request.parsedRequest.url + ".proto"
      case None => protobuf.root / request.parsedRequest.url + ".proto"
    }
    if (!protoFile.exists) {
      throw new AeromockProtoFileNotFoundException(protoFile.toString)
    }

    val response = createResponseData(project, request.parsedRequest)
    val variableHelper = new VariableHelper(VariableManager.getRequestMap ++ VariableManager.getOriginalVariableMap().asScala)

    val apiTypeName = response._1.get(naming.`type`) match {
      case Some(apiTypeName: String) if StringUtils.isNotBlank(apiTypeName) => apiTypeName
      case _ => throw new AeromockProtoTypeNotSpecifiedException(request.parsedRequest.url)
    }

    val filteredMap = scanMap(response._1 - naming.`type`)(variableHelper.variableConversion)

    if (request.queryString.contains(s"${project._naming.debug}=true")) {
      val dumperOptions = new DumperOptions
      dumperOptions.setDefaultFlowStyle(DumperOptions.FlowStyle.FLOW)

      renderYaml(new Yaml(dumperOptions).dumpAsMap(asJavaMap(filteredMap)(nop)), HttpResponseStatus.OK)
    } else {

      val parser = new AeromockProtoParser(protobuf.root)
      val result = parser.parseProto(protoFile)

      val fields = result.types.get(apiTypeName) match {
        case None => throw new AeromockProtoTypeNotFoundException(apiTypeName)
        case Some(value) => value.map(f => createProtoProxyValue(filteredMap, result.dependencyTypes, f))
      }
      // TODO 型チェック
      renderProtobuf(ProtoProxyObject(fields).toByteArray, HttpResponseStatus.OK, response._2)
    }

  }

  private def createProtoProxyValue(dataMap: Map[Any, Any],
      dependencies: Map[String, List[ProtoField]], f: ProtoField): ProtoProxyValue[_, _] = {

    import jp.co.cyberagent.aeromock.protobuf.ProtoFieldLabel._
    import jp.co.cyberagent.aeromock.protobuf.ProtoFieldType._

    val value = dataMap.get(f.name) match {
      case Some(v) => v
      case None if f.label == OPTIONAL => f.defaultValue.get
      case _ => throw new RuntimeException("値が指定されてない") // TODO
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
      case f @ ProtoField(REPEATED, t, _, tag, _) => {
        t match {
          case tt @ DOUBLE => ProtoProxyListValue(tt, value.asInstanceOf[List[Double]], tag)
          case tt @ FLOAT => ProtoProxyListValue(tt, value.asInstanceOf[List[Float]], tag)
          case tt @ INT32 => ProtoProxyListValue(tt, value.asInstanceOf[List[Int]], tag)
          case tt @ INT64 => ProtoProxyListValue(tt, value.asInstanceOf[List[Long]], tag)
          case tt @ UINT32 => ProtoProxyListValue(tt, value.asInstanceOf[List[Int]], tag)
          case tt @ UINT64 => ProtoProxyListValue(tt, value.asInstanceOf[List[Long]], tag)
          case tt @ SINT32 => ProtoProxyListValue(tt, value.asInstanceOf[List[Int]], tag)
          case tt @ SINT64 => ProtoProxyListValue(tt, value.asInstanceOf[List[Long]], tag)
          case tt @ FIXED32 => ProtoProxyListValue(tt, value.asInstanceOf[List[Int]], tag)
          case tt @ FIXED64 => ProtoProxyListValue(tt, value.asInstanceOf[List[Long]], tag)
          case tt @ SFIXED32 => ProtoProxyListValue(tt, value.asInstanceOf[List[Int]], tag)
          case tt @ SFIXED64 => ProtoProxyListValue(tt, value.asInstanceOf[List[Long]], tag)
          case tt @ BOOL => ProtoProxyListValue(tt, value.asInstanceOf[List[Boolean]], tag)
          case tt @ STRING => ProtoProxyListValue(tt, value.asInstanceOf[List[String]], tag)
          case tt @ BYTES => ProtoProxyListValue(tt, value.asInstanceOf[List[String]].map(ByteString.copyFromUtf8(_)), tag) // TODO
          case _ => throw new RuntimeException("unsupported type") // TODO
        }
      }
      case f @ ProtoField(_, t, _, tag, _) => {
        t match {
          case tt @ DOUBLE => ProtoProxySingleValue(tt, value.asInstanceOf[Double], tag)
          case tt @ FLOAT => ProtoProxySingleValue(tt, value.asInstanceOf[Float], tag)
          case tt @ INT32 => ProtoProxySingleValue(tt, value.asInstanceOf[Int], tag)
          case tt @ INT64 => ProtoProxySingleValue(tt, value.asInstanceOf[Long], tag)
          case tt @ UINT32 => ProtoProxySingleValue(tt, value.asInstanceOf[Int], tag)
          case tt @ UINT64 => ProtoProxySingleValue(tt, value.asInstanceOf[Long], tag)
          case tt @ SINT32 => ProtoProxySingleValue(tt, value.asInstanceOf[Int], tag)
          case tt @ SINT64 => ProtoProxySingleValue(tt, value.asInstanceOf[Long], tag)
          case tt @ FIXED32 => ProtoProxySingleValue(tt, value.asInstanceOf[Int], tag)
          case tt @ FIXED64 => ProtoProxySingleValue(tt, value.asInstanceOf[Long], tag)
          case tt @ SFIXED32 => ProtoProxySingleValue(tt, value.asInstanceOf[Int], tag)
          case tt @ SFIXED64 => ProtoProxySingleValue(tt, value.asInstanceOf[Long], tag)
          case tt @ BOOL => ProtoProxySingleValue(tt, value.asInstanceOf[Boolean], tag)
          case tt @ STRING => ProtoProxySingleValue(tt, value.asInstanceOf[String], tag)
          case tt @ BYTES => ProtoProxySingleValue(tt, ByteString.copyFromUtf8(value.asInstanceOf[String]), tag)
          case _ => throw new RuntimeException("unsupported type") // TODO
        }
      }
    }
  }
}
