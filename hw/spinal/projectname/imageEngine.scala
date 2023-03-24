package projectname
import spinal.core._
import spinal.lib._

import scala.io.Source
import scala.language.postfixOps
case class imageEngine() extends Component {
  val io = new Bundle {
    val pixel_addr = in(UInt(log2Up(1024) bits))
    val pixel_data = out(Bits(8 bits))
    val uart_din = in(Bits(8 bits))
    val uart_d_valid = in(Bool())
    val led = out(Reg(Bits(6 bits)) init B"6'b111111")
  }
  val fileContent_handle = Source.fromFile("output.hex").getLines().map(BigInt(_, 16)).toArray

  def file_content = for (sampleIndex <- 0 until 1024) yield {
    B(fileContent_handle(sampleIndex))
  }

  // 下面代码等价于 reg [7:0] mem [4:0];initial $readmemh("output.hex",mem);
  // val fileContents = Source.fromFile("output.hex").getLines().map(BigInt(_, 16)).toSeq
  val image_ram = Mem(Bits(8 bits), initialContent = file_content)

  val wr_index = Reg(UInt(log2Up(1024) bits)) init 0

  when(wr_index === 0) {
    io.pixel_data := image_ram(io.pixel_addr)
  } otherwise {
    io.pixel_data := B(0)
  }

  val wr_index_rst_cnt = Counter(27000000 / 100) // 计数100ms

  wr_index_rst_cnt.increment()
  when(wr_index_rst_cnt.willOverflow) {
    wr_index := 0
  }

  when(io.uart_d_valid.edge()) {
    wr_index_rst_cnt.clear()
  }

  object State extends SpinalEnum {
    val wait_data, wait_next = newElement()
  }
  val state = RegInit(State.wait_data)

  switch(state) {
    is(State.wait_data) {
      when(io.uart_d_valid) {
        image_ram(wr_index) := io.uart_din
        io.led := ~io.uart_din(0, 6 bits)
        wr_index := wr_index + 1
        state := State.wait_next
      }
    }
    is(State.wait_next) {
      when(!io.uart_d_valid) {
        state := State.wait_data
      }
    }
  }
}
