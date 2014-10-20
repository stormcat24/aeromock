package jp.co.cyberagent.aeromock

import com.google.protobuf.ByteString

/**
 *
 * @author stormcat24
 */
package object protobuf {


  def getStringBytes(value: String): ByteString = ByteString.copyFromUtf8(value)

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
