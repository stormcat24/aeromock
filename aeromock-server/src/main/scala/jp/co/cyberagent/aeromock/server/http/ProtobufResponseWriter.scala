package jp.co.cyberagent.aeromock.server.http

import com.google.protobuf.ByteString
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{HttpResponseStatus, HttpResponse, FullHttpRequest}
import jp.co.cyberagent.aeromock.core.el.VariableHelper
import jp.co.cyberagent.aeromock.core.http.VariableManager
import jp.co.cyberagent.aeromock.{AeromockLoadDataException, AeromockSystemException, AeromockApiNotFoundException}
import jp.co.cyberagent.aeromock.config.Project
import jp.co.cyberagent.aeromock.helper._
import jp.co.cyberagent.aeromock.helper.DeepTraversal._
import jp.co.cyberagent.aeromock.data.{CommonDataHelper, DataFileReaderFactory, DataPathResolver}
import jp.co.cyberagent.aeromock.protobuf._
import scaldi.Injector
import scala.collection.JavaConverters._
import scalaz._

/**
 * [[jp.co.cyberagent.aeromock.server.http.HttpRequestProcessor]] for Google Protocol Buffers.
 * @author stormcat24
 */
class ProtobufResponseWriter(implicit inj: Injector) extends HttpRequestProcessor
  with HttpResponseWriter with ResponseStatusSupport {

  val project = inject[Project]

  /**
   *
   * @param request [[FullHttpRequest]]
   * @param context [[ChannelHandlerContext]]
   * @return [[HttpResponse]]
   */
  override def process(request: FullHttpRequest)(implicit context: ChannelHandlerContext): HttpResponse = {
    val dataRoot = project._data.root
    val protobufRoot = project._protobuf.root
    val naming = project._naming

    val dataFile = DataPathResolver.resolve(dataRoot, request.parsedRequest, naming) match {
      case None => throw new AeromockApiNotFoundException(request.parsedRequest.url)
      case Some(file) => file
    }

    val dataMap = DataFileReaderFactory.create(dataFile) match {
      case None => throw new AeromockSystemException(s"Cannot read Data file '${dataFile.toString}'")
      case Some(reader) => reader.readFile(dataFile).collect {
        case (key, value) => (key, value)
      }.toMap
    }

    val commonDataHelper = new CommonDataHelper(naming)
    val commonMergedMap = commonDataHelper.getMergedDataMap(dataRoot, project.dataScript)
    val mergedMap = commonDataHelper.mergeAdditional(commonMergedMap, dataMap)

    val customResponse = createCustomResponse(naming, mergedMap) match {
      case Success(value) => value
      case Failure(e) => {
        val errors = e.list.map(_.getMessage)
        throw new AeromockLoadDataException(NonEmptyList(errors.head, errors.drop(1): _*))
      }
    }
    val reducedMap = mergedMap - project._naming.response
    val variableHelper = new VariableHelper(VariableManager.getRequestMap ++ VariableManager.getOriginalVariableMap().asScala)
    val filteredMap = scanMap(reducedMap)(variableHelper.variableConversion)

    val protoFile = protobufRoot / "api" / request.parsedRequest.url + ".proto"
    if (!protoFile.exists) {
      // TODO exception
      throw new AeromockSystemException("not found proto")
    }

    // TODO templateと同居させるべきか？（併用するシーンは考えにくいので別でいい気がする）
    val parser = new AeromockProtoParser(protobufRoot)
    val result = parser.parseProto(protoFile)
    // TODO rootのprotoを特定する仕組みが必要(prefix対応)

    val rootType = result.types.get(result.types.keys.head).get
//    // TODO default値の設定
//    // TODO optionalなら無くてもOK
//    // TODO 型チェック

    val fields = rootType.map(f => createProtoProxyValue(filteredMap, result.dependencyTypes, f))
    renderProtobuf(ProtoProxyObject(fields).toByteArray, HttpResponseStatus.OK, customResponse)
  }

  private def createProtoProxyValue(dataMap: Map[Any, Any],
      dependencies: Map[String, List[ProtoField]], f: ProtoField): ProtoProxyValue[_, _] = {

    import jp.co.cyberagent.aeromock.protobuf.ProtoFieldType._
    import jp.co.cyberagent.aeromock.protobuf.ProtoFieldLabel._

    (dataMap.get(f.name), f) match {
      case (None, ProtoField(REQUIRED, _, _, _, _)) => {
        throw new RuntimeException(s"${f.name} is required.")
      }
      case (None, ProtoField(_, t, name, tag, doc)) => {
        //          val value = t.defaultValue
        throw new RuntimeException("TODO")
      }
      case (Some(value), ProtoField(label, m @ MESSAGE(typeName), name, tag, doc)) => {
        val depFields = dependencies.get(typeName) match {
          case None => throw new RuntimeException(s"${typeName} not exist")
          case Some(value) => value
        }
        val depList = depFields.map(createProtoProxyValue(value.asInstanceOf[Map[Any, Any]], dependencies, _))
        ProtoProxyMessageValue(m, depList, tag)
      }
      case (Some(value), f @ ProtoField(REPEATED, t, _, tag, _)) => {
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
          case tt @ BYTES => ProtoProxyListValue(tt, value.asInstanceOf[List[ByteString]], tag) // TODO ByteString変換
          case _ => throw new RuntimeException("unsupported type")
        }
      }
      case (Some(value), f @ ProtoField(_, t, _, tag, _)) => {
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
          case tt @ BYTES => ProtoProxySingleValue(tt, value.asInstanceOf[ByteString], tag) // TODO ByteString変換
          case _ => throw new RuntimeException("unsupported type")
        }
      }
    }
  }
}
