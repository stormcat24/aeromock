package jp.co.cyberagent.aeromock

import com.google.protobuf.{ByteString, CodedOutputStream}

import scala.language.experimental
import scala.reflect.ClassTag
import jp.co.cyberagent.aeromock.helper._
import scalaz._
import Scalaz._

/**
 *
 * @author stormcat24
 */
package object protobuf {

  def getByteString(value: String): ByteString = ByteString.copyFromUtf8(value)

  def cast[A](value: Any)(implicit tag: ClassTag[A]): Either[Throwable, A] = {

    value match {
      case string: CharSequence => doCast[A](string.toString)
      case i: Number => doCast[A](i.toString)
    }
  }

  val TypeString = classOf[String]
  val TypeInt = classOf[Int]
  val TypeLong = classOf[Long]
  val TypeFloat = classOf[Float]
  val TypeDouble = classOf[Double]

  private[protobuf] def doCast[A](value: String)(implicit tag: ClassTag[A]): Either[Throwable, A] = {
    trye((implicitly[ClassTag[A]].runtimeClass match {
      case TypeString => value.toString
      case TypeInt => value.toInt
      case TypeLong => value.toLong
      case TypeFloat => value.toFloat
      case TypeDouble => value.toDouble
    }).asInstanceOf[A])
  }


  case class ProtoField(
    label: ProtoFieldLabel,
    `type`: ProtoFieldType[_],
    name: String,
    tag: Int
  )

  case class ProtoProxyObject(
    values: List[ProtoProxyValue[_, _]]
  ) {

    lazy val serializedSize: Int = values.foldLeft(0)((left, right) => left + right.serizliedSize)

    def toByteArray(): Array[Byte] = {
      val result = new Array[Byte](serializedSize)
      val output = CodedOutputStream.newInstance(result)
      values.map(_.write(output))
      output.checkNoSpaceLeft
      result
    }
  }

  trait ProtoProxyValue[A, B] {
    val field: ProtoFieldType[A]
    val value: B

    def serizliedSize: Int
    def write(output: CodedOutputStream): Unit
  }

  case class ProtoProxySingleValue[A] (
    field: ProtoFieldType[A],
    value: A,
    tag: Int
  ) extends ProtoProxyValue[A, A] {
    override def serizliedSize: Int = field.computeSize(tag, value)
    override def write(output: CodedOutputStream): Unit = field.write(output, tag, value)
  }

  case class ProtoProxyListValue[A] (
    field: ProtoFieldType[A],
    value: List[A],
    tag: Int
  ) extends ProtoProxyValue[A, List[A]] {
    override def serizliedSize: Int = {
      value.foldLeft(0)((left, right) => left + field.computeSizeNoTag(right)) + value.size
    }
    override def write(output: CodedOutputStream): Unit = value.map(field.write(output, tag, _))
  }

  case class ProtoProxyMessageValue[A] (
    field: ProtoFieldType[A],
    value: List[ProtoProxyValue[_, _]],
    tag: Int
  ) extends ProtoProxyValue[A, List[ProtoProxyValue[_, _]]] {
    override def serizliedSize: Int = {
      value.foldLeft(0)((left, right) => left + right.serizliedSize)
    }
    override def write(output: CodedOutputStream): Unit = {
      value.map(_.write(output))
    }

  }

}


