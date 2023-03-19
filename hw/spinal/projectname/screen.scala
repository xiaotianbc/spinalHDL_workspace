package projectname

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Random
class screen(reset_delay: Int = 2700000) extends Component {
  val io = new Bundle {
    val screen_sck = out(RegInit(False))
    val screen_mosi = out(RegInit(False))
    val screen_rstn = out(RegInit(False))
    val screen_dc = out(RegInit(False))
    val screen_cs = out(RegInit(True))
  }
  noIoPrefix()

  val fileContent_handle = Source.fromFile("output.hex").getLines().map(BigInt(_, 16)).toArray
  def file_content = for (sampleIndex <- 0 until 1024) yield {
    B(fileContent_handle(sampleIndex))
  }
  // 下面代码等价于 reg [7:0] mem [4:0];initial $readmemh("output.hex",mem);
  // val fileContents = Source.fromFile("output.hex").getLines().map(BigInt(_, 16)).toSeq
  val image_rom = Mem(Bits(8 bits), initialContent = file_content)
  val image_index = Reg(UInt(log2Up(1024) bits)) init 0




  val setup_cmds = Cat(
    B"8'hAE", // display off
    B"8'h81", // contast value to 0x7F according to datasheet
    B"8'h7F",
    B"8'hA6", // normal screen mode (not inverted)
    B"8'h20", // horizontal addressing mode
    B"8'h00",
    B"8'hC8", // normal scan direction
    B"8'h40", // first line to start scanning from
    B"8'hA1", // address 0 is segment 0
    B"8'hA8", // mux ratio
    B"8'h3f", // 63 (64 -1)
    B"8'hD3", // display offset
    B"8'h00", // no offset
    B"8'hD5", // clock divide ratio
    B"8'h80", // set to default ratio/osc frequency
    B"8'hD9", // set precharge
    B"8'h22", // switch precharge to 0x22 default
    B"8'hDB", // vcom deselect level
    B"8'h20", // 0x20
    B"8'h8D", // charge pump config
    B"8'h14", // enable charge pump
    B"8'hA4", // resume RAM content
    B"8'hAF" // display on
  )
  // 宽度：23*8
  val init_cmds_idx = RegInit(U(23))
  // val data_to_send = setup_cmds((init_cmds_idx - 1) * 8, 8 bits) // x(offset, width bits)

  val spi_dr = Reg(Bits(8 bits)) init 0
  val bits_to_send = RegInit(U(7))
  val spi_div_cnt = Counter(4)
  object State extends SpinalEnum {
    val init_reset, load_cmds_init, send_data, check_finished, send_pixel = newElement()
  }
  val state = RegInit(State.init_reset)

  val counter = Counter(reset_delay * 4)
  switch(state) {
    is(State.init_reset) {
      counter.increment()
      when(counter.value < reset_delay) {
        io.screen_rstn := True // 先等待电源稳定
      } elsewhen (counter.value < 2 * reset_delay) {
        io.screen_rstn := False // 复位
      } elsewhen (counter.value < 3 * reset_delay) {
        io.screen_rstn := True // 取消复位，等待稳定后接受命令
      } otherwise {
        state := State.load_cmds_init
        counter.clear()
      }
    }
    is(State.load_cmds_init) {
      spi_dr := setup_cmds((init_cmds_idx - 1) * 8, 8 bits)
      init_cmds_idx := init_cmds_idx - 1
      state := State.send_data
      io.screen_cs := False
      io.screen_dc := False // DC=0 命令
      bits_to_send := 7
      spi_div_cnt.clear()
    }
    is(State.send_data) {
      spi_div_cnt.increment()
      when(spi_div_cnt.value === 0) {
        io.screen_sck := False
        io.screen_mosi := spi_dr(bits_to_send)
      }
      when(spi_div_cnt.value === 2) {
        io.screen_sck := True
        bits_to_send := bits_to_send - 1
        when(bits_to_send === 0) {
          state := State.check_finished
        }
      }
    }
    is(State.check_finished) {
      io.screen_cs := True // 取消片选，不是必须的
      when(init_cmds_idx === 0) {
        state := State.send_pixel
      } otherwise {
        state := State.load_cmds_init
      }
    }
    is(State.send_pixel) {
      spi_dr := image_rom(image_index)
      image_index := image_index + 1
      state := State.send_data
      io.screen_cs := False
      io.screen_dc := True // DC=0 命令
      bits_to_send := 7
      spi_div_cnt.clear()
    }
  }
}

object screen extends App {
  Config.spinal.generateVerilog(new screen())
}
object screenSim extends App {
  Config.sim.compile(new screen(reset_delay = 100)).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    for (idx <- 0 to 9999) {
      // Drive the dut inputs with random values
      //      dut.io.cond0.randomize()
      //      dut.io.cond1.randomize()

      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()

    }
  }
}
