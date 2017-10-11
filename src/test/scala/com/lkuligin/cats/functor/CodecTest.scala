package com.lkuligin.cats.functor

import com.lkuligin.cats.functors.Codec
import org.scalatest.{MustMatchers, WordSpec}
import com.lkuligin.cats.functors.Codec._

class CodecTest extends WordSpec with MustMatchers {

  final case class Box[A](value: A)

  implicit def boxCodec[A](implicit codec: Codec[A]) = codec.imap[Box[A]](Box(_), _.value)

  "box codec via imap" in {
    encode(Box(1)) mustBe "1"
    decode[Box[Int]]("2") mustBe Some(Box(2))
  }
}
