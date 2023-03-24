package projectname

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Random
class uart_rx(sys_freq: Int = 100000000, baudrate: Int = 1000000) extends Component {
  val io = new Bundle {
    val rxp = in(Bool())
    val dout = out(Reg(Bits(8 bits)) init 0)
    val d_valid = out(RegInit(False))
  }
  noIoPrefix()

  val BAUD_MAX = sys_freq / baudrate
  val baud_cnt = Counter(BAUD_MAX)
  val rx_index = Reg(UInt(log2Up(8) bits)) init 0
  object State extends SpinalEnum {
    val idle, start_bit, data_bits, stop_bit = newElement()
  }
  val state = RegInit(State.idle)
  switch(state) {
    is(State.idle) {
      when(io.rxp === False) {
        state := State.start_bit
        baud_cnt.clear()
        io.d_valid := False
      }
    }
    is(State.start_bit) {
      baud_cnt.increment()
      when(baud_cnt.value === BAUD_MAX / 2) {
        state := State.data_bits
        baud_cnt.clear()
      }
    }
    is(State.data_bits) {
      baud_cnt.increment()
      when(baud_cnt.willOverflow) {
        io.dout := io.rxp.asBits ## io.dout(7 downto 1)
        rx_index := rx_index + 1
        when(rx_index === 7) {
          state := State.stop_bit
          baud_cnt.clear()
        }
      }
    }
    is(State.stop_bit) {
      baud_cnt.increment()
      when(baud_cnt.willOverflow) {
        state := State.idle
        when(io.rxp === True) {
          io.d_valid := True
        }
      }
    }
  }
}

object uart_rx extends App {
  Config.spinal.generateVerilog(new uart_rx())
}

object uart_rxSim extends App {
  def tx_data(data: Int, dut: uart_rx): Unit = {
    for (idx <- 0 to 9) {
      if (idx == 0) {
        dut.io.rxp #= false
      } else if (idx == 9) {
        dut.io.rxp #= true
      } else {
        dut.io.rxp #= ((data & (1 << (idx - 1))) != 0)
      }
      // 等待 BAUD_MAX 个时钟周期
      for (_ <- 0 until dut.BAUD_MAX) {
        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()
      }
    }
  }

  // 建立仿真 其中RiseCounter 来创建代码
  SimConfig.withWave.doSim(new uart_rx) { dut =>
    // 创建时钟
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)
    // 注意 io的电平赋值用 #=
    dut.io.rxp #= true
    // 等待10个时钟周期，不知道是否有更好的写法
    for (idx <- 0 to 10) {
      dut.clockDomain.waitRisingEdge()
    }

    tx_data(0x55, dut)
    tx_data(0xaa, dut)

    for (idx <- 0 to 999) {
      dut.io.rxp #= true
      // Drive the dut inputs with random values
      // dut.io.rxp #= Random.nextBoolean()
      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
    }
  }
}
