package projectname

import spinal.core._
import spinal.lib._
class RiseCounter extends Component {
  val io = new Bundle {
    val sigIn = in Bool()
    val clear = in Bool()
    val cnt = out UInt (32 bits)
  }
  val counter = new Area {
    val cnt = Counter(32 bits, io.sigIn.rise(False))
    when(io.clear) {
      cnt.value.clearAll()
    }
    io.cnt := cnt.value
  }
}

//Generate the RiseCounter's Verilog
object RiseCounterVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new RiseCounter)
  }
}
