package axi

import chisel3._
import chisel3.util._
import chisel3.internal.firrtl.Width

class AxiAddr(val addrWidth: Int, val dataWidth: Int, val idWidth: Int, val userWidth: Int)
    extends Bundle {
  // required fields
  val addr = UInt(addrWidth.W)
  val id = UInt(idWidth.W)
  val size = UInt(3.W) // beatBytes = 2^size
  val len = UInt(8.W) // beatsPerBurst - 1. Max 255 for INCR, 15 otherwise
  val burst = UInt(2.W) // burst type: 0=fixed, 1=incr, 2=wrap

  // optional fields
  val cache = UInt(4.W)
  val lock = Bool()
  val prot = UInt(3.W)
  val qos = UInt(4.W)
  val region = UInt(4.W)
  val user = UInt(userWidth.W)

  def initDefault() = {
    id := 0.U
    size := log2Ceil(dataWidth / 8).U
    len := 1.U
    burst := 1.U
    cache := 0.U
    lock := false.B
    prot := 0.U
    qos := 0.U
    region := 0.U
    user := 0.U
  }
}

object AxiAddr {
  def apply(addrWidth: Int, dataWidth: Int, idWidth: Int = 1, userWidth: Int = 1) = {
    new AxiAddr(addrWidth = addrWidth,
                dataWidth = dataWidth,
                idWidth = idWidth,
                userWidth = userWidth)
  }
}

abstract class AxiData(val dataWidth: Int, val userWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val last = Bool()
  val user = UInt(userWidth.W) // optional

  def initDefault() = {
    user := 0.U
  }

  // used for "downcasting" Axi(Read|Write)Data
  def getId: Option[UInt] = None
  def getStrb: Option[UInt] = None
}

class AxiReadData(dataWidth: Int, val idWidth: Int, userWidth: Int)
    extends AxiData(dataWidth = dataWidth, userWidth = userWidth) {
  val id = UInt(idWidth.W)
  val resp = UInt(2.W)

  override def initDefault() = {
    id := 0.U
  }

  override def getId = Some(id)
}

object AxiReadData {
  def apply(dataWidth: Int, idWidth: Int = 1, userWidth: Int = 1) = {
    new AxiReadData(dataWidth = dataWidth, idWidth = idWidth, userWidth = userWidth)
  }
}

class AxiWriteData(dataWidth: Int, userWidth: Int)
    extends AxiData(dataWidth = dataWidth, userWidth = userWidth) {
  val strb = UInt((dataWidth / 8).W)

  override def initDefault() = {
    strb := (Math.pow(2, dataWidth).toInt - 1).U
  }

  override def getStrb = Some(strb)
}

object AxiWriteData {
  def apply(dataWidth: Int, userWidth: Int = 1) = {
    new AxiWriteData(dataWidth = dataWidth, userWidth = userWidth)
  }
}

class AxiWriteResp(val idWidth: Int, val userWidth: Int) extends Bundle {
  val id = UInt(idWidth.W)
  val resp = UInt(2.W)
  val user = UInt(userWidth.W) // optional

  def initDefault() = {
    id := 0.U
    user := 0.U
  }
}

object AxiWriteResp {
  def apply(idWidth: Int = 1, userWidth: Int = 1) = {
    new AxiWriteResp(idWidth = idWidth, userWidth = userWidth)
  }
}

trait AxiReadAddr;
trait AxiWriteAddr;

class AxiSlave(val addrWidth: Int,
               val dataWidth: Int,
               val idWidth: Int = 1,
               val arUserWidth: Int = 1,
               val rUserWidth: Int = 1,
               val awUserWidth: Int = 1,
               val wUserWidth: Int = 1,
               val bUserWidth: Int = 1)
    extends Bundle {
  val readAddr = Flipped(
    Decoupled(
      new AxiAddr(addrWidth = addrWidth,
                  idWidth = idWidth,
                  dataWidth = dataWidth,
                  userWidth = arUserWidth) with AxiReadAddr))
  val readData = Decoupled(
    new AxiReadData(dataWidth = dataWidth, idWidth = idWidth, userWidth = rUserWidth))

  val writeAddr = Flipped(
    Decoupled(
      new AxiAddr(addrWidth = addrWidth,
                  idWidth = idWidth,
                  dataWidth = dataWidth,
                  userWidth = awUserWidth) with AxiWriteAddr))
  val writeData = Flipped(
    Decoupled(new AxiWriteData(dataWidth = dataWidth, userWidth = wUserWidth)))
  val writeResp = Decoupled(AxiWriteResp(idWidth = idWidth, userWidth = bUserWidth))

  def initDefault() = {
    readData.bits.initDefault
    writeResp.bits.initDefault
  }
}

object AxiSlave {
  def apply(addrWidth: Int,
            dataWidth: Int,
            idWidth: Int = 1,
            arUserWidth: Int = 1,
            rUserWidth: Int = 1,
            awUserWidth: Int = 1,
            wUserWidth: Int = 1,
            bUserWidth: Int = 1) = {
    new AxiSlave(
      addrWidth = addrWidth,
      dataWidth = dataWidth,
      idWidth = idWidth,
      arUserWidth = arUserWidth,
      rUserWidth = rUserWidth,
      awUserWidth = awUserWidth,
      wUserWidth = wUserWidth,
      bUserWidth = bUserWidth
    )
  }
}

class AxiMaster(val addrWidth: Int,
                val dataWidth: Int,
                val idWidth: Int = 1,
                val arUserWidth: Int = 1,
                val rUserWidth: Int = 1,
                val awUserWidth: Int = 1,
                val wUserWidth: Int = 1,
                val bUserWidth: Int = 1)
    extends Bundle {
  val readAddr = Decoupled(
    new AxiAddr(addrWidth = addrWidth,
                dataWidth = dataWidth,
                idWidth = idWidth,
                userWidth = arUserWidth) with AxiReadAddr)
  val readData = Flipped(
    Decoupled(AxiReadData(dataWidth = dataWidth, idWidth = idWidth, userWidth = rUserWidth)))

  val writeAddr = Decoupled(
    new AxiAddr(addrWidth = addrWidth,
                dataWidth = dataWidth,
                idWidth = idWidth,
                userWidth = awUserWidth) with AxiWriteAddr)
  val writeData = Decoupled(AxiWriteData(dataWidth = dataWidth, userWidth = wUserWidth))
  val writeResp = Flipped(Decoupled(AxiWriteResp(idWidth = idWidth, userWidth = bUserWidth)))

  def initDefault() = {
    readAddr.bits.initDefault
    writeAddr.bits.initDefault
    writeData.bits.initDefault
  }

  def setupRead(srcAddr: UInt, numBeats: UInt) = {
    readAddr.bits.addr := srcAddr
    readAddr.bits.len := numBeats
    readAddr.valid := true.B
    readData.ready := true.B
    when(readAddr.ready) { readAddr.valid := false.B }
    when(readData.bits.last) { readData.ready := false.B }
  }

  def setupRead(srcAddr: UInt, numBits: Width): Unit = {
    setupRead(srcAddr, (numBits.get / dataWidth min 1).U)
  }
}

object AxiMaster {
  def apply(addrWidth: Int,
            dataWidth: Int,
            idWidth: Int = 1,
            arUserWidth: Int = 1,
            rUserWidth: Int = 1,
            awUserWidth: Int = 1,
            wUserWidth: Int = 1,
            bUserWidth: Int = 1) = {
    new AxiMaster(
      addrWidth = addrWidth,
      dataWidth = dataWidth,
      idWidth = idWidth,
      arUserWidth = arUserWidth,
      rUserWidth = rUserWidth,
      awUserWidth = awUserWidth,
      wUserWidth = wUserWidth,
      bUserWidth = bUserWidth
    )
  }
}
