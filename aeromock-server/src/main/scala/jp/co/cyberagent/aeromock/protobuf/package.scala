package jp.co.cyberagent.aeromock

import java.io.File
import java.nio.file.Path

import com.google.protobuf.ByteString
import com.squareup.protoparser.ProtoSchemaParser
import jp.co.cyberagent.aeromock.helper._
import scala.collection.JavaConverters._

/**
 *
 * @author stormcat24
 */
package object protobuf {


  def getStringBytes(value: String): ByteString = ByteString.copyFromUtf8(value)

}

class AeromockProtoParser(protobufRoot: Path) {

  def parseProto(protoFile: Path) {

    val result = ProtoSchemaParser.parse(protoFile.toFile)
    val allDeps = fetchDependencies(result.getDependencies.asScala.toSet)
    println(allDeps)

    println("finish")
  }

  def fetchDependencies(deps: Set[String]): Set[String] = {
    // TODO 存在チェックすべきか？
    deps ++ deps.map(protobufRoot / _).filter(_.exists).flatMap(dep => {
      val result = ProtoSchemaParser.parse(dep.toFile)
      fetchDependencies(result.getDependencies.asScala.toSet)
    })
  }
}

case class ObjectDef(
  name: String,
  messageType: MessageType,
  value: Any,
  index: Int,
  required: Boolean
)


object MessageType {
  case object Double extends MessageType("double")
  case object Float extends MessageType("float")
  case object Int32 extends MessageType("int32")
  case object Int64 extends MessageType("int64")
  case object UInt32 extends MessageType("uint32")
  case object UInt64 extends MessageType("uint64")
  case object SInt32 extends MessageType("2int32")
  case object SInt64 extends MessageType("2int64")
  case object Fixed32 extends MessageType("fixed32")
  case object Fixed64 extends MessageType("fixed64")
  case object SFixed32 extends MessageType("sfixed32")
  case object SFixed64 extends MessageType("sfixed64")
  case object Bool extends MessageType("bool")
  case object String extends MessageType("string")
  case object Bytes extends MessageType("bytes")
}

sealed abstract class MessageType(val value: String) {

}
