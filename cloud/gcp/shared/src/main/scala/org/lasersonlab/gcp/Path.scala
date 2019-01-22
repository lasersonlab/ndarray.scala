package org.lasersonlab.gcp

//import org.lasersonlab.uri.?
//import org.lasersonlab.gcp.googleapis.Paged
//
//sealed trait Path
//
//case class File(
//  bucket: Bucket,
//  path: Vector[String]
//)
//extends Path
//
//sealed trait Prefix extends Path
//
//case class Bucket(
//  name: String,
//  prefixes: ?[Paged[Dir]] = None,
//  files: ?[Paged[File]] = None
//)
//extends Prefix
//
//case class Dir(
//  bucket: Bucket,
//  path: Vector[String],
//  prefixes: ?[Paged[Dir]] = None,
//  files: ?[Paged[File]] = None
//)
//extends Prefix
