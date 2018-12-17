package axi

import scala.util.Random

import chisel3._
import chisel3.iotesters.{Driver, PeekPokeTester}
import org.scalatest.{FreeSpec, Matchers}

object AxiMapperTest {
  val AddrWidth = 32
  val DmaWidth = 16
  val UserWidth = 12
  val BurstMaxBeats = 256
  val DmaBytes = DmaWidth / 8
  val Rng = new Random(42)
}
import AxiMapperTest._

abstract class AxiMapperTest[M <: AxiMapper](c: M, numBytes: Int) extends PeekPokeTester(c) {
  val ReadAddr = BigInt(AddrWidth, Rng)

  var beatsLeft = Math.ceil(numBytes.toDouble / DmaBytes).toInt
  val numBursts = Math.ceil(beatsLeft.toDouble / BurstMaxBeats).toInt
  var addr = ReadAddr

  // ddr not ready, stream not valid
  poke(c.io.mm.data.ready, false)
  poke(c.io.mm.addr.ready, false)
  poke(c.io.s.valid, false)

  // set up params, but don't make valid
  poke(c.io.params.bits.addr, ReadAddr)
  poke(c.io.params.bits.len, numBytes)
  poke(c.io.params.valid, false)

  step(1)
  expect(c.io.params.ready, true)

  // send params
  poke(c.io.params.valid, true)
  step(1)
  poke(c.io.params.valid, false)
  expect(c.io.params.ready, false)

  // def setup(): Unit
  def sendPre(): Unit
  def sendInner(): Unit
  def sendPost(): Unit

  for (_ <- 0 until numBursts) {
    val burstBeats = Math.min(beatsLeft, BurstMaxBeats)

    // expect addr is correct
    expect(c.io.mm.addr.valid, true)
    expect(c.io.mm.addr.bits.addr, addr)
    expect(c.io.mm.addr.bits.len, burstBeats - 1)
    poke(c.io.mm.addr.ready, true)

    // accept addr, enter data state
    poke(c.io.mm.addr.ready, true)
    sendPre()
    step(1)
    poke(c.io.mm.addr.ready, false)

    for (_ <- 0 until burstBeats) {
      sendInner()
      beatsLeft -= 1
    }
    step(1)
    addr += burstBeats * DmaBytes
  }
  step(1)
  expect(c.io.params.ready, true)
  sendPost()
}

class AxiMM2STest(c: AxiMM2S, numBytes: Int) extends AxiMapperTest(c, numBytes) {
  def sendPre() = {
    poke(c.io.mm.data.valid, true)
    poke(c.io.s.ready, true)
  }

  def sendInner() = {
    val readData = BigInt(DmaWidth, Rng)
    val userData = BigInt(UserWidth, Rng)
    val last = beatsLeft == 1
    poke(c.io.mm.data.bits.data, readData)
    poke(c.io.mm.data.bits.user, userData)
    poke(c.io.mm.data.bits.last, last)
    poke(c.io.mm.data.valid, true)
    step(1)
    expect(c.io.s.valid, true)
    expect(c.io.s.bits.data, readData)
    expect(c.io.s.bits.last, last)
    expect(c.io.params.ready, false)
  }

  def sendPost() = {
    expect(c.io.s.valid, false)
    expect(c.io.mm.data.ready, false)
  }
}

class AxiMM2SBoundaryConds(c: AxiMM2S) extends PeekPokeTester(c) {
  val ReadAddr = BigInt(AddrWidth, Rng)
  val NumBytes = 6
  poke(c.io.mm.data.valid, false)
  poke(c.io.mm.addr.ready, true)

  // set up params, immediately make valid
  poke(c.io.params.bits.addr, ReadAddr)
  poke(c.io.params.bits.len, NumBytes)
  poke(c.io.params.valid, true)

  poke(c.io.s.ready, true)

  step(1) // wait params -> wait addr

  expect(c.io.s.valid, false)
  expect(c.io.params.ready, false)

  // expect addr is correct
  var beatsLeft = Math.ceil(NumBytes.toDouble / DmaBytes).toInt
  expect(c.io.mm.addr.valid, true)
  expect(c.io.mm.addr.bits.addr, ReadAddr)
  expect(c.io.mm.addr.bits.len, beatsLeft - 1)
  poke(c.io.mm.addr.ready, true)

  // set up bogus params
  poke(c.io.params.bits.addr, 999)
  poke(c.io.params.bits.len, 999)

  step(1) // wait addr -> data

  expect(c.io.mm.data.ready, true)
  expect(c.io.s.ready, true)

  for (_ <- 0 until beatsLeft) {
    val readData = BigInt(DmaWidth, Rng)
    val userData = BigInt(UserWidth, Rng)
    val last = beatsLeft == 1
    poke(c.io.mm.data.valid, true)
    poke(c.io.mm.data.bits.data, readData)
    poke(c.io.mm.data.bits.user, userData)
    poke(c.io.mm.data.bits.last, last)
    step(1)
    expect(c.io.s.valid, true)
    expect(c.io.s.bits.data, readData)
    expect(c.io.s.bits.last, last)
    expect(c.io.params.ready, false)
    beatsLeft -= 1
  }
  step(1)
  expect(c.io.s.valid, false)
  expect(c.io.params.ready, true)
  expect(c.io.mm.data.ready, false)
}

class AxiS2MMTest(c: AxiS2MM, numBytes: Int) extends AxiMapperTest(c, numBytes) {
  expect(c.io.mm.data.valid, false)

  def sendPre() = {
    poke(c.io.s.valid, true)
    poke(c.io.mm.data.ready, true)
  }

  def sendInner() = {
    val writeData = BigInt(DmaWidth, Rng)
    val userData = BigInt(UserWidth, Rng)
    val strb = BigInt(DmaBytes, Rng)
    val last = beatsLeft == 1
    poke(c.io.s.bits.data, writeData)
    poke(c.io.s.bits.last, last)
    poke(c.io.s.bits.user, userData)
    poke(c.io.s.bits.strb, strb)
    poke(c.io.s.valid, true)
    step(1)
    expect(c.io.mm.data.valid, true)
    expect(c.io.mm.data.bits.data, writeData)
    expect(c.io.mm.data.bits.last, last)
    expect(c.io.mm.data.bits.getStrb.get, strb)
    expect(c.io.params.ready, false)
  }

  def sendPost() = {
    expect(c.io.mm.data.valid, false)
    expect(c.io.s.ready, false)
  }
}

class AxiMapperSpec extends FreeSpec with Matchers {
  val m = AxiMaster(addrWidth = AddrWidth,
                    dataWidth = DmaWidth,
                    rUserWidth = UserWidth,
                    wUserWidth = UserWidth)

  "AxiMM2S" - {
    def DutFactory() = new AxiMM2S(m.readAddr.bits, m.readData.bits)
    def TestFactory[T <: AxiMapperTest[AxiMM2S]](numBytes: Int) =
      Driver(DutFactory) { new AxiMM2STest(_, numBytes) } should be(true)

    "partially full, aligned burst" in TestFactory(4 * DmaBytes)
    "partially full, unaligned burst" in TestFactory(4 * DmaWidth / 8 + 3)
    "single full burst" in TestFactory(BurstMaxBeats * DmaBytes)
    "full burst + partial" in TestFactory(BurstMaxBeats * DmaBytes + 2 * DmaBytes)
    "multiple full burst" in TestFactory(2 * BurstMaxBeats * DmaBytes)

    "hold params during tx" in {
      Driver(DutFactory) { new AxiMM2SBoundaryConds(_) } should be(true)
    }
  }

  "AxiS2MM" - {
    def DutFactory() = new AxiS2MM(m.writeAddr.bits, m.writeData.bits)
    def TestFactory[T <: AxiMapperTest[AxiS2MM]](numBytes: Int) =
      Driver(DutFactory) { new AxiS2MMTest(_, numBytes) } should be(true)

    "partially full, aligned burst" in TestFactory(4 * DmaBytes)
    "partially full, unaligned burst" in TestFactory(4 * DmaWidth / 8 + 3)
    "single full burst" in TestFactory(BurstMaxBeats * DmaBytes)
    "full burst + partial" in TestFactory(BurstMaxBeats * DmaBytes + 2 * DmaBytes)
    "multiple full burst" in TestFactory(2 * BurstMaxBeats * DmaBytes)
  }
}
