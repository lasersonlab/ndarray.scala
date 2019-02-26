package lasersonlab

import org.lasersonlab.files.Uri
import org.lasersonlab.zarr.{ Compressor, utils }
import org.lasersonlab.zarr.dtype.ByteOrder._
import org.lasersonlab.{ zarr ⇒ z }

import scala.concurrent.Future

object zarr
  extends z.int
     with hammerlab.bytes.syntax
     with lasersonlab.future
     with lasersonlab.slist
     with z.io.syntax
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

  type Bytes = hammerlab.bytes.Bytes

  type Path = Uri
   val Path = Uri

  type ChunkSize = z.utils.ChunkSize
   val ChunkSize = z.utils.ChunkSize
}
