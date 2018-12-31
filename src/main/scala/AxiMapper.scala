package axi

import chisel3._
import chisel3.core.SpecifiedDirection
import chisel3.util._
import chisel3.experimental.ChiselEnum

object AxiDma {
  val MaxBurstLen = 0x100
  val DefaultMaxTxLen = 0x7fffff // default of Xilinx DataMover IP (23 bits)
}
import AxiDma._

class AxiDmaParams(val addrWidth: Int, val maxTxLen: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val len = UInt(maxTxLen.U.getWidth.W)
}

object AxiDmaParams {
  def apply(addrWidth: Int, maxTxLen: Int) = {
    new AxiDmaParams(addrWidth = addrWidth, maxTxLen = maxTxLen)
  }
}

object AxiDmaState extends ChiselEnum {
  val WaitParams, SendAddr, SendData = Value
}
import AxiDmaState._

class AxiMapperIO(streamT: AxiStream,
                  addrT: AxiAddr,
                  dataT: AxiData,
                  dataDirection: SpecifiedDirection,
                  val maxTxLen: Int)
    extends Bundle {
  def ddt() = Decoupled(dataT.cloneType)
  def dst() = Decoupled(streamT.cloneType)

  val mm = new Bundle {
    val addr = Decoupled(addrT.cloneType)
    val data = if (dataDirection == SpecifiedDirection.Input) Flipped(ddt) else ddt
  }
  val s = if (dataDirection == SpecifiedDirection.Input) dst else Flipped(dst)
  val params = Flipped(Decoupled(AxiDmaParams(addrWidth = addrT.addrWidth, maxTxLen = maxTxLen)))

  override def cloneType: this.type =
    new AxiMapperIO(streamT, addrT, dataT, dataDirection, maxTxLen).asInstanceOf[this.type]
}

abstract class AxiMapper(val streamT: AxiStream,
                         val addrT: AxiAddr,
                         val dataT: AxiData,
                         val dataDirection: SpecifiedDirection,
                         val maxTxLen: Int)
    extends Module {
  val ddstr = if (dataDirection == SpecifiedDirection.Input) "Input" else "Output"

  val dataBytes = dataT.dataWidth / 8
  val state = RegInit(WaitParams)

  val io = IO(new AxiMapperIO(streamT, addrT, dataT, dataDirection, maxTxLen))

  val loadParams = io.params.valid && state === WaitParams
  val addr = RegEnable(io.params.bits.addr, loadParams)
  val inBeats = ceilDiv(io.params.bits.len, dataBytes)
  val beatsLeft = RegEnable(inBeats, loadParams)
  val burstBeatsLeft = Reg(UInt(MaxBurstLen.U.getWidth.W))

  io.params.ready := state === WaitParams

  io.mm.addr.bits.initDefault
  io.mm.addr.bits.addr := addr
  io.mm.addr.bits.len := (beatsLeft - 1.U).min(0xff.U)
  io.mm.addr.valid := state === SendAddr || (state === SendData && burstBeatsLeft === 0.U)

  switch(state) {
    is(WaitParams) {
      when(io.params.valid) {
        burstBeatsLeft := inBeats.min(MaxBurstLen.U)
        state := SendAddr
      }
    }

    is(SendAddr) {
      when(io.mm.addr.ready) {
        state := SendData
      }
    }

    is(SendData) {
      val didTransfer = if (dataDirection == SpecifiedDirection.Input) {
        io.s.ready && io.mm.data.valid
      } else {
        io.mm.data.ready && io.s.valid
      }
      when(didTransfer) {
        burstBeatsLeft := burstBeatsLeft - 1.U
        beatsLeft := beatsLeft - 1.U
        addr := addr + dataBytes.U
        when(burstBeatsLeft === 0.U) {
          burstBeatsLeft := beatsLeft.min(MaxBurstLen.U) - 1.U
          state := Mux(beatsLeft === 0.U, WaitParams, SendData)
        }
      }
    }
  }
}

/** An AXI MM2S interface which supports multi-burst reads.
  *
  * @param addrT the AXI master read addr bundle type
  * @param dataT the AXI master read data bundle type
  * @param maxTxLen the maximum number of bytes in a transaction
  *
  * IO Bundle:
  *  mm.addr, mm.data - the addr/data channel (connect to AXI master)
  *  s - the output AxiStream bundle
  *  params - AxiDmaParams which describes the tx. `params.ready` is asserted when the tx is done.
  */
class AxiMM2S(addrT: AxiAddr with AxiReadAddr, dataT: AxiReadData, maxTxLen: Int = DefaultMaxTxLen)
    extends AxiMapper(
      AxiStream(dataT.dataWidth, userWidth = dataT.userWidth, idWidth = dataT.idWidth),
      addrT,
      dataT,
      SpecifiedDirection.Input,
      maxTxLen) {
  io.s.bits.initDefault
  io.s.valid := io.mm.data.valid && state === SendData
  io.s.bits.data := io.mm.data.bits.data
  io.s.bits.id := io.mm.data.bits.getId.get
  io.s.bits.user := io.mm.data.bits.user
  io.s.bits.last := io.mm.data.bits.last
  io.mm.data.ready := io.s.ready && state === SendData
}

object AxiMM2S {
  def apply(mm: AxiMaster, maxTxLen: Int = DefaultMaxTxLen) = {
    val mm2s = Module(new AxiMM2S(mm.readAddr.bits, mm.readData.bits, maxTxLen))
    mm2s.io.mm.addr <> mm.readAddr
    mm2s.io.mm.data <> mm.readData
    mm2s
  }
}

/** An AXI S2MM interface which supports multi-burst writes.
  *
  * @param addrT the AXI master read addr bundle type
  * @param dataT the AXI master read data bundle type
  * @param maxTxLen the maximum number of bytes in a transaction
  *
  * IO Bundle:
  *  mm.addr, mm.data - the addr/data channel (connect to AXI master)
  *  s - the output AxiStream bundle
  *  params - AxiDmaParams which describes the tx. `params.ready` is asserted when the tx is done.
  */
class AxiS2MM(addrT: AxiAddr with AxiWriteAddr,
              dataT: AxiWriteData,
              maxTxLen: Int = DefaultMaxTxLen)
    extends AxiMapper(AxiStream(dataT.dataWidth, userWidth = dataT.userWidth),
                      addrT,
                      dataT,
                      SpecifiedDirection.Output,
                      maxTxLen) {
  io.s.ready := io.mm.data.ready && state === SendData
  io.mm.data.bits.data := io.s.bits.data
  io.mm.data.bits.user := io.s.bits.user
  io.mm.data.bits.last := io.s.bits.last
  io.mm.data.bits.getStrb.get := io.s.bits.strb
  io.mm.data.valid := io.s.valid && state === SendData
}

object AxiS2MM {
  def apply(mm: AxiMaster, maxTxLen: Int = DefaultMaxTxLen) = {
    val s2mm = Module(new AxiS2MM(mm.writeAddr.bits, mm.writeData.bits, maxTxLen))
    s2mm.io.mm.addr <> mm.writeAddr
    s2mm.io.mm.data <> mm.writeData
    mm.writeResp.ready := true.B
    s2mm
  }
}
