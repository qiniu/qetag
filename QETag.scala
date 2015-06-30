package main

import java.io.{File, FileInputStream}
import java.io.InputStream
import java.security.MessageDigest

object QEtag {
  private val BLOCK_BITS = 22 // Indicate that the blocksize is 4M
  private val BLOCK_SIZE = 1 << BLOCK_BITS

  def apply(fname: String) = etag(fname)
  def apply(in: InputStream, size: Long) = etag(in, size)

  def etag(fname: String): String = {
    val file = new File(fname)
    val in = new FileInputStream(file)
    try {
      etag(in, file.length)
    } finally {
      if (in != null) {
        in.close()
      }
    }
  }

  def etag(in: InputStream, size: Long): String = {
    val sha1 = MessageDigest.getInstance("SHA-1")
    blockCount(size) match {
      case 1 => {
        val buf = new Array[Byte](size.toInt)
        require(in.read(buf) == size)
        urlSafeBase64Encode(0x16.toByte +: sha1.digest(buf))
      }
      case _ => {
        val buf = new Array[Byte](BLOCK_SIZE)
        val sha1Tmp = MessageDigest.getInstance("SHA-1")
        Stream.continually(in.read(buf))
          .takeWhile(_ != -1)
          .foreach(n => {
            sha1.update(sha1Tmp.digest(buf.take(n)))
          })
        urlSafeBase64Encode(0x96.toByte +: sha1.digest())
      }
    }
  }

  private def blockCount(fsize: Long) = ((fsize + (BLOCK_SIZE - 1)) >> BLOCK_BITS).toInt

  private def urlSafeBase64Encode(data: Array[Byte]) = {
    val b64Encoder = new sun.misc.BASE64Encoder()
    val encodedData = b64Encoder.encode(data)
    val encodedString = new String(encodedData)
    encodedString.replace('+', '-').replace('/', '_')
  }
}

object Main {
  def main(args: Array[String]) {
    if (args.length != 1) {
      System.err.println("Usage: qetag <filename>")
      System.exit(1)
    }
    println(QEtag(args(0)))
  }
}
