package lasersonlab

import cats.effect.IO
import org.lasersonlab.uri.Uri
import org.lasersonlab.zarr.{ Compressor, utils }
import org.lasersonlab.zarr.dtype.ByteOrder._
import org.lasersonlab.{ zarr ⇒ z }

object zarr
  extends z.int
     with hammerlab.bytes.syntax
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

  type Path[F[_]] = Uri[F]
   val Path    = Uri

  type ChunkSize = z.utils.ChunkSize
   val ChunkSize = z.utils.ChunkSize
}
