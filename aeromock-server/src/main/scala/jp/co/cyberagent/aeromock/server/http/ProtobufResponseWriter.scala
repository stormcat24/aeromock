package jp.co.cyberagent.aeromock.server.http

import com.squareup.protoparser.ProtoSchemaParser
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{HttpResponse, FullHttpRequest}
import jp.co.cyberagent.aeromock.core.el.VariableHelper
import jp.co.cyberagent.aeromock.core.http.VariableManager
import jp.co.cyberagent.aeromock.{AeromockProtoParser, AeromockSystemException, AeromockApiNotFoundException}
import jp.co.cyberagent.aeromock.config.Project
import jp.co.cyberagent.aeromock.helper._
import jp.co.cyberagent.aeromock.data.{DataFileReaderFactory, DataPathResolver}
import jp.co.cyberagent.aeromock.protobuf._
import scaldi.Injector
import scala.collection.JavaConverters._

/**
 * [[jp.co.cyberagent.aeromock.server.http.HttpRequestProcessor]] for Google Protocol Buffers.
 * @author stormcat24
 */
class ProtobufResponseWriter(implicit inj: Injector) extends HttpRequestProcessor with HttpResponseWriter {

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
      case Some(reader) => reader.readFile(dataFile)
    }

    val variableHelper = new VariableHelper(VariableManager.getRequestMap ++ VariableManager.getOriginalVariableMap().asScala)

    val protoFile = protobufRoot / "api" / request.parsedRequest.url + ".proto"
    if (!protoFile.exists) {
      // TODO exception
      throw new AeromockSystemException("not found proto")
    }

    val parser = new AeromockProtoParser(protobufRoot)
    parser.parseProto(protoFile)

    ???
  }
}
