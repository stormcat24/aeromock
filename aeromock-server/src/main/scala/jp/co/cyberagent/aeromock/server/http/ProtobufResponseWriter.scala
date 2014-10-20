package jp.co.cyberagent.aeromock.server.http

import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{HttpResponse, FullHttpRequest}
import scaldi.Injector

/**
 * [[jp.co.cyberagent.aeromock.server.http.HttpRequestProcessor]] for Google Protocol Buffers.
 * @author stormcat24
 */
class ProtobufResponseWriter(implicit inj: Injector) extends HttpRequestProcessor with HttpResponseWriter {

  /**
   *
   * @param request [[FullHttpRequest]]
   * @param context [[ChannelHandlerContext]]
   * @return [[HttpResponse]]
   */
  override def process(request: FullHttpRequest)(implicit context: ChannelHandlerContext): HttpResponse = {
    // TODO
    ???
  }
}
