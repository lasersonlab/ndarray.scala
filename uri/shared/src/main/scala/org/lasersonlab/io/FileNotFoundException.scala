package org.lasersonlab.io

import java.io.IOException

import org.lasersonlab.uri.Uri

case class FileNotFoundException(path: Uri) extends IOException
