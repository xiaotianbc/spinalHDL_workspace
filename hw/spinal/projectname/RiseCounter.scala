package projectname

import spinal.core._
import spinal.lib._
class RiseCounter extends Component {
  val io = new Bundle {
    val led= out(Reg(UInt(4 bits)) init U"1111")
  }
  noIoPrefix()
  val cnt=Counter(50000000)
  cnt.increment()
  when(cnt.willOverflow){
    io.led:=io.led-1
  }
}

//Generate the RiseCounter's Verilog
object RiseCounterVerilog extends App {
  Config.spinal.generateVerilog(new RiseCounter())
}
