package net.ghoula.sarati.codec

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import java.util.UUID

import net.ghoula.sarati.ast.xml.{XmlNode, qname}

/** [[Encoder]] instances to [[XmlNode]] from primitives, `java.time` types, `UUID`, and the common
  * collection shapes.
  *
  * XML has no scalar node kinds, so scalars encode as `Text` and `Option` maps `None` to an empty
  * `Text` — the same rules as [[AstBuilder]]. Numbers print via `Long` when whole and decimal
  * otherwise; `java.time` types and `UUID` print via `toString`. Collections encode as an `<array>`
  * element with one child per element, and string-keyed maps as an `<object>` element with one
  * child element per key — the shapes the XML decoder layer reads back.
  *
  * Known composition limits, both consequences of wrapping: `None` encodes as an empty `Text` and
  * does not round-trip (the scalar decoder sees `""`), and a collection used as a case-class field
  * nests the `<array>`/`<object>` wrapper inside the field element, which the decoders do not
  * unwrap — collections round-trip standalone but not as fields.
  */
object XmlEncoders {

  private def text(s: String): XmlNode = XmlNode.Text(s)

  private def fromNumber(n: Double): XmlNode = text(n match {
    case _ if n.isWhole && n.abs <= Long.MaxValue.toDouble => n.toLong.toString
    case _ => n.toString
  })

  given Encoder[String, XmlNode] = new Encoder[String, XmlNode] {
    def encode(value: String): XmlNode = text(value)
  }

  given Encoder[Int, XmlNode] = new Encoder[Int, XmlNode] {
    def encode(value: Int): XmlNode = text(value.toString)
  }

  given Encoder[Long, XmlNode] = new Encoder[Long, XmlNode] {
    def encode(value: Long): XmlNode = text(value.toString)
  }

  given Encoder[Double, XmlNode] = new Encoder[Double, XmlNode] {
    def encode(value: Double): XmlNode = fromNumber(value)
  }

  given Encoder[Boolean, XmlNode] = new Encoder[Boolean, XmlNode] {
    def encode(value: Boolean): XmlNode = text(value.toString)
  }

  given Encoder[Byte, XmlNode] = new Encoder[Byte, XmlNode] {
    def encode(value: Byte): XmlNode = text(value.toString)
  }

  given Encoder[Short, XmlNode] = new Encoder[Short, XmlNode] {
    def encode(value: Short): XmlNode = text(value.toString)
  }

  given Encoder[Float, XmlNode] = new Encoder[Float, XmlNode] {
    def encode(value: Float): XmlNode = fromNumber(value.toDouble)
  }

  given Encoder[BigInt, XmlNode] = new Encoder[BigInt, XmlNode] {
    def encode(value: BigInt): XmlNode = text(value.toString)
  }

  given Encoder[BigDecimal, XmlNode] = new Encoder[BigDecimal, XmlNode] {
    def encode(value: BigDecimal): XmlNode = text(value.toString)
  }

  given Encoder[Instant, XmlNode] = new Encoder[Instant, XmlNode] {
    def encode(value: Instant): XmlNode = text(value.toString)
  }

  given Encoder[LocalDate, XmlNode] = new Encoder[LocalDate, XmlNode] {
    def encode(value: LocalDate): XmlNode = text(value.toString)
  }

  given Encoder[LocalDateTime, XmlNode] = new Encoder[LocalDateTime, XmlNode] {
    def encode(value: LocalDateTime): XmlNode = text(value.toString)
  }

  given Encoder[LocalTime, XmlNode] = new Encoder[LocalTime, XmlNode] {
    def encode(value: LocalTime): XmlNode = text(value.toString)
  }

  given Encoder[OffsetDateTime, XmlNode] = new Encoder[OffsetDateTime, XmlNode] {
    def encode(value: OffsetDateTime): XmlNode = text(value.toString)
  }

  given Encoder[ZonedDateTime, XmlNode] = new Encoder[ZonedDateTime, XmlNode] {
    def encode(value: ZonedDateTime): XmlNode = text(value.toString)
  }

  given Encoder[UUID, XmlNode] = new Encoder[UUID, XmlNode] {
    def encode(value: UUID): XmlNode = text(value.toString)
  }

  given [A] => (encoder: Encoder[A, XmlNode]) => Encoder[Option[A], XmlNode] =
    new Encoder[Option[A], XmlNode] {
      def encode(value: Option[A]): XmlNode = encodeEval(value).run()

      override def encodeEval(value: Option[A]): Eval[XmlNode] =
        Eval.defer {
          value match {
            case None => Eval.now(text(""))
            case Some(a) => encoder.encodeEval(a)
          }
        }
    }

  given [A] => (encoder: Encoder[A, XmlNode]) => Encoder[List[A], XmlNode] =
    new Encoder[List[A], XmlNode] {
      def encode(value: List[A]): XmlNode = encodeEval(value).run()

      override def encodeEval(value: List[A]): Eval[XmlNode] =
        Encoder.drainElements[A, XmlNode, XmlNode](
          value,
          encoder.encodeEval,
          finish = children => XmlNode.Element(qname("array"), List.empty, children)
        )
    }

  given [A] => (encoder: Encoder[A, XmlNode]) => Encoder[Seq[A], XmlNode] =
    new Encoder[Seq[A], XmlNode] {
      def encode(value: Seq[A]): XmlNode = encodeEval(value).run()

      override def encodeEval(value: Seq[A]): Eval[XmlNode] =
        Encoder.drainElements[A, XmlNode, XmlNode](
          value.toList,
          encoder.encodeEval,
          finish = children => XmlNode.Element(qname("array"), List.empty, children)
        )
    }

  given [A] => (encoder: Encoder[A, XmlNode]) => Encoder[Vector[A], XmlNode] =
    new Encoder[Vector[A], XmlNode] {
      def encode(value: Vector[A]): XmlNode = encodeEval(value).run()

      override def encodeEval(value: Vector[A]): Eval[XmlNode] =
        Encoder.drainElements[A, XmlNode, XmlNode](
          value.toList,
          encoder.encodeEval,
          finish = children => XmlNode.Element(qname("array"), List.empty, children)
        )
    }

  given [A] => (encoder: Encoder[A, XmlNode]) => Encoder[Map[String, A], XmlNode] =
    new Encoder[Map[String, A], XmlNode] {
      def encode(value: Map[String, A]): XmlNode = encodeEval(value).run()

      override def encodeEval(value: Map[String, A]): Eval[XmlNode] =
        Encoder.drainElements[(String, A), XmlNode, XmlNode](
          value.toList,
          encodeElem = { case (key, a) =>
            encoder.encodeEval(a).map(fv => XmlNode.Element(qname(key), List.empty, List(fv)))
          },
          finish = children => XmlNode.Element(qname("object"), List.empty, children)
        )
    }
}
