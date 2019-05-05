package com.spotright.common.util

import java.nio.ByteBuffer

object Hexdumper {

  /**
    * Returns a String containing a hexdump of the specified bytes.
    *
    * @param data the byte array containing the bytes to display
    * @param start the offset into the byte array to begin displaying
    * @param length the number of bytes to display
    * @param blocksize the number of bytes to display in each block
    * @return a String containing a human-readable representation of the specified bytes.
    */
  def hexdump(data: Array[Byte], start: Int = 0, length: Int = -1, blocksize: Int = 0): String = {

    def formHeader(block: Int, bytesInBlock: Int): String = {
      List(
        s"Block #$block : $bytesInBlock byte block",
        "+---OFFSETS----+",
        // hex(7X) - 2 sp - decimal(7d) - 2sp - hexdata(48s) - 6 sp - asciidata(16s)
        "HEX      DECIMAL  HEX DATA                                             ASCII DATA"
      ).mkString("\n")
    }

    require(start >= 0, "start < 0")
    require(length >= -1, "length < -1")
    require(blocksize >= 0, "blocksize < 0")

    val dataLength = if (length == -1) data.length else length
    val dataBlocksize = if (blocksize == 0) dataLength else blocksize

    if (length == 0) return ""

    // Invariants.
    // * Each block has a header
    // * Each output dump line consists of an offset marker, 2x 8-byte hex groups, an ascii group
    // * If blocksize % 8 != 0 then fill a line as far as possible

    val blocks =
    // Break data into blocksize elements, tracking element index
      data.slice(start, start + dataLength).grouped(dataBlocksize).zipWithIndex.map {
        case (bs, blockidx) =>
          val displayedBlock = blockidx + 1 // Nominal block.  Humans don't like base-0
        val header = formHeader(displayedBlock, bs.length)

          val lines =
          // Break block into lines, tracking line index
            bs.grouped(16).zipWithIndex.map {
              case (linebs, lineidx) =>
                val offset = blockidx * dataBlocksize + lineidx * 16
                val prefix = f"$offset%07X  $offset%07d"

                val (hexen, asciis) =
                  linebs.grouped(8).map {
                    chunk =>
                      val hex = chunk.map { b => f"$b%02X" }.mkString(" ")

                      val ascii = chunk.map {
                        b =>
                          if (b >= 32 && b <= 126) b.toChar else '.'
                      }.mkString

                      (hex, ascii)
                  }.toList.unzip

                // The hex component is fixed width
                val hexStr = f"${hexen.mkString("  ")}%-48s"

                // prefix(7X - 2 sp - 7d) - 2sp - hexStr(48s) - 6 sp - asciis(16s)
                s"$prefix  $hexStr     ${asciis.mkString}"
            }

          (Iterator.single(header) ++ lines).mkString("\n")
      }

    blocks.mkString("\n\n")
  }

  /**
    * @return a String containing a hexdump of the contents of the specified ByteBuffer.
    */
  def hexdump(byteBuffer: ByteBuffer): String =
    hexdump(
      byteBuffer.array,
      byteBuffer.arrayOffset + byteBuffer.position,
      byteBuffer.limit - byteBuffer.position - byteBuffer.arrayOffset)
}
