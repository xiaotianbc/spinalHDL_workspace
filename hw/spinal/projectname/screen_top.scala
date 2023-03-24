package projectname

import spinal.core._
import spinal.core.sim._
import spinal.core.sim.SimConfig


case class screen_top(top_reset_delay: Int = 2700000) extends Component {
  val io = new Bundle {
    val screen_sck = out(Bool())
    val screen_mosi = out(Bool())
    val screen_rstn = out(Bool())
    val screen_dc = out(Bool())
    val screen_cs = out(Bool())
    val uart_rxp = in(Bool())
    val led = out(Bits(6 bits))
  }
  noIoPrefix()

  val u_screen = new screen(reset_delay = top_reset_delay)
  io.screen_rstn <> u_screen.io.screen_rstn
  io.screen_sck <> u_screen.io.screen_sck
  io.screen_mosi <> u_screen.io.screen_mosi
  io.screen_dc <> u_screen.io.screen_dc
  io.screen_cs <> u_screen.io.screen_cs

  val u_imageEngine = new imageEngine()
  u_imageEngine.io.pixel_addr <> u_screen.io.pixel_addr
  u_screen.io.pixel_data <> u_imageEngine.io.pixel_data
  io.led <> u_imageEngine.io.led

  val u_uart_rx = new uart_rx(sys_freq = 25000000, baudrate = 115200)
  u_uart_rx.io.rxp <> io.uart_rxp
  u_imageEngine.io.uart_din <> u_uart_rx.io.dout
  u_imageEngine.io.uart_d_valid <> u_uart_rx.io.d_valid
}

object screen_top extends App {
  Config.spinal.generateVerilog(new screen_top())
}

object screen_topSim extends App {
  def tx_data(data: Int, dut: screen_top): Unit = {
    for (idx <- 0 to 9) {
      if (idx == 0) {
        dut.io.uart_rxp #= false
      } else if (idx == 9) {
        dut.io.uart_rxp #= true
      } else {
        dut.io.uart_rxp #= ((data & (1 << (idx - 1))) != 0)
      }
      // 等待 BAUD_MAX 个时钟周期
      for (_ <- 0 until dut.u_uart_rx.BAUD_MAX) {
        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()
      }
    }
  }

  // 建立仿真 其中RiseCounter 来创建代码
  SimConfig.withWave.doSim(new screen_top(top_reset_delay = 10)) { dut =>
    // 创建时钟
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)
    // 注意 io的电平赋值用 #=
    dut.io.uart_rxp #= true
    // 等待10个时钟周期，不知道是否有更好的写法
    for (idx <- 0 to 100) {
      dut.clockDomain.waitRisingEdge()
    }

    tx_data(0x01, dut)
    tx_data(0x02, dut)
    tx_data(0x03, dut)

    for (idx <- 0 to 9999) {
      dut.io.uart_rxp #= true
      // Drive the dut inputs with random values
      // dut.io.rxp #= Random.nextBoolean()
      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
    }
  }
}
