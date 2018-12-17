package axi

import chisel3._
import chisel3.util._

object ceilDiv {
  def apply(a: UInt, b: Int) = {
    require(isPow2(b))
    val hasPartialByte = (a & (b - 1).U) =/= 0.U
    (a >> log2Ceil(b)) + hasPartialByte
  }
}
