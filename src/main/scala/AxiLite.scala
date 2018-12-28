package axi

import chisel3._
import chisel3.core.BundleLitBinding
import chisel3.util._

class AxiLiteAddr(val addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val prot = UInt(3.W) // optional, but included by vivado

  def Lit(addr: UInt, prot: UInt = 0.U) = {
    val clone = cloneType
    clone.selfBind(
      BundleLitBinding(
        Map(
          clone.addr -> litArgOfBits(addr),
          clone.prot -> litArgOfBits(prot),
        )))
    clone
  }
}

object AxiLiteAddr {
  def apply(addrWidth: Int) = new AxiLiteAddr(addrWidth)
}

class AxiLiteWriteData(val dataWidth: Int) extends Bundle {
  require(dataWidth == 32 || dataWidth == 64, "AxiLite `dataWidth` must be 32 or 64")
  val data = UInt(dataWidth.W)
  val strb = UInt((dataWidth / 8).W)

  def Lit(data: UInt, strb: UInt = (Math.pow(2, dataWidth).toInt - 1).U) = {
    val clone = cloneType
    clone.selfBind(
      BundleLitBinding(
        Map(
          clone.data -> litArgOfBits(data),
          clone.strb -> litArgOfBits(strb),
        )))
    clone
  }
}

object AxiLiteWriteData {
  def apply(dataWidth: Int) = new AxiLiteWriteData(dataWidth)
}

class AxiLiteReadData(val dataWidth: Int) extends Bundle {
  require(dataWidth == 32 || dataWidth == 64, "AxiLite `dataWidth` must be 32 or 64")
  val data = UInt(dataWidth.W)
  val resp = UInt(2.W)

  def Lit(data: UInt, resp: UInt = 0.U) = {
    val clone = cloneType
    clone.selfBind(
      BundleLitBinding(
        Map(
          clone.data -> litArgOfBits(data),
          clone.resp -> litArgOfBits(resp),
        )))
    clone
  }
}

object AxiLiteReadData {
  def apply(dataWidth: Int) = new AxiLiteReadData(dataWidth)
}

class AxiLiteSlave(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val readAddr = Flipped(Decoupled(AxiLiteAddr(addrWidth)))
  val readData = Decoupled(AxiLiteReadData(dataWidth))

  val writeAddr = Flipped(Decoupled(AxiLiteAddr(addrWidth)))
  val writeData = Flipped(Decoupled(AxiLiteWriteData(dataWidth)))
  val writeResp = Decoupled(UInt(2.W))
}

object AxiLiteSlave {
  def apply(addrWidth: Int, dataWidth: Int) =
    new AxiLiteSlave(addrWidth = addrWidth, dataWidth = dataWidth)
}

class AxiLiteMaster(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val readAddr = Decoupled(AxiLiteAddr(addrWidth))
  val readData = Flipped(Decoupled(AxiLiteReadData(dataWidth)))

  val writeAddr = Decoupled(AxiLiteAddr(addrWidth))
  val writeData = Decoupled(AxiLiteWriteData(dataWidth))
  val writeResp = Flipped(Decoupled(UInt(2.W)))
}

object AxiLiteMaster {
  def apply(addrWidth: Int, dataWidth: Int) =
    new AxiLiteMaster(addrWidth = addrWidth, dataWidth = dataWidth)
}
