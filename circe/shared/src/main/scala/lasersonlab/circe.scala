package lasersonlab

import org.lasersonlab.circe.{ EitherCodec, InstantCodec, SingletonCodec }

object circe
extends EitherCodec
   with SingletonCodec
   with InstantCodec
