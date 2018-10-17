package lasersonlab

import org.lasersonlab.zarr.{ Compressor, utils }
import org.lasersonlab.zarr.dtype.ByteOrder._
import org.lasersonlab.{ zarr ⇒ z }

object zarr
  extends z.int
    with lasersonlab.slist
    with utils.slist.Codecs {
  object long extends z.long
  object order {
    implicit val < = LittleEndian
    implicit val > : Endianness = BigEndian
    implicit val | = None
  }
  object compress {
    implicit val    - = Compressor.None
    implicit val zlib = Compressor.ZLib()
  }
}
