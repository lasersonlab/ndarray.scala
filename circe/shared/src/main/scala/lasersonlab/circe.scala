package lasersonlab

import org.lasersonlab.circe.{ EitherDecoder, EitherEncoder, SingletonCodec }

object circe
extends EitherDecoder
   with EitherEncoder
   with SingletonCodec
