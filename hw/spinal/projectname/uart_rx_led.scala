package projectname

import spinal.core._

import scala.language.postfixOps
case class uart_rx_led() extends Component {

  val io = new Bundle {
    val rxp = in(Bool())
    val led = out(Bits(4 bits))
  }
  noIoPrefix()

  val dout = Reg(Bits(8 bits)) init B"11111111"

  val u_uart_rx = new uart_rx(sys_freq = 100000000, baudrate = 5000000)
  u_uart_rx.io.rxp <> io.rxp
  when(u_uart_rx.io.d_valid) {
    dout := u_uart_rx.io.dout
  }

  io.led := ~dout(0, 4 bits)

}

object uart_rx_led extends App{
  Config.spinal.generateVerilog(new uart_rx_led())
}
