package jp.co.cyberagent.aeromock.protobuf

import java.nio.file.Paths

import com.google.protobuf.{CodedOutputStream, ByteString}
import com.squareup.protoparser.ProtoSchemaParser
import protobuf.schema.UserOuterClass

/**
 *
 * @author stormcat24
 */
object ProtoTest {

  def main(args: Array[String]) {
    val protofile = Paths.get("/Users/a13062/.ghq/github.com/CyberAgent/aeromock/tutorial/protobuf/protobuf/api/test.proto")
    val result = ProtoSchemaParser.parse(protofile.toFile)
    println(result.getTypes)

    // TODO importを巡回
    // TODO 重複は除去
//    println(result)
//    println(result.getPackageName)

    val map = Map(
      "user1" -> Map(
        "id" -> 1,
        "name" -> "hoge1",
        "description" -> "hoge1 hogehoge"
      ),
      "user2" -> Map(
        "id" -> 2,
        "name" -> "hoge2",
        "description" -> "hoge2 hogehoge"
      )
    )

    var binField = 0
    binField = binField | 0x00000001
    binField = binField | 0x00000002
    binField = binField | 0x00000004

    val builder1 = UserOuterClass.User.newBuilder()
    val builder2 = UserOuterClass.User.newBuilder()
    val user1 = builder1.setId(1).setName("hoge1").setDescription("hoge1 hogehoge").build

    val size = getSize(binField, user1)

    val array = new Array[Byte](size)
    val output = CodedOutputStream.newInstance(array)

    output.writeUInt32(1, user1.getId)
    output.writeBytes(2, getBytes(user1.getName))
    output.writeBytes(3, getBytes(user1.getDescription))
    // TODO ?
//    getUnknownFields().writeTo(output);
    println(array.toList)

    //    final byte[] result = new byte[getSerializedSize()];
//    final CodedOutputStream output = CodedOutputStream.newInstance(result);


    println(user1.toByteArray.toList)
//    val user2 = builder2.setId(2).setName("hoge2").setDescription("hoge2 hogehoge").build
//
//    val test = protobuf.api.Test.TestResponse.newBuilder().setUser1(user1).setUser2(user2).build
//    val data = test.toByteArray
//    println(data.toList)

//    builder.set
  }

//  public void writeTo(com.google.protobuf.CodedOutputStream output)
//  throws java.io.IOException {
//    getSerializedSize();
//    if (((bitField0_ & 0x00000001) == 0x00000001)) {
//      output.writeUInt32(1, id_);
//    }
//    if (((bitField0_ & 0x00000002) == 0x00000002)) {
//      output.writeBytes(2, getNameBytes());
//    }
//    if (((bitField0_ & 0x00000004) == 0x00000004)) {
//      output.writeBytes(3, getDescriptionBytes());
//    }
//    getUnknownFields().writeTo(output);
//  }


  private def getSize(bitField0: Int, user: UserOuterClass.User): Int = {

    var size = 0
    if (((bitField0 & 0x00000001) == 0x00000001)) {
      size += com.google.protobuf.CodedOutputStream.computeUInt32Size(1, user.getId)
    }


    if (((bitField0 & 0x00000002) == 0x00000002)) {
      size += com.google.protobuf.CodedOutputStream.computeBytesSize(2, getBytes(user.getName))
    }


    if (((bitField0 & 0x00000004) == 0x00000004)) {
      size += com.google.protobuf.CodedOutputStream.computeBytesSize(3, getBytes(user.getDescription))
    }

// TODO ?
//    size += getUnknownFields.getSerializedSize
    size
  }

  /**
   * <code>required string name = 2;</code>
   */
  def getBytes(str: String): ByteString = com.google.protobuf.ByteString.copyFromUtf8(str)
}
