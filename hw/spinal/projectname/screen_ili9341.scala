package projectname

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

case class screen_ili9341(reset_cnt: Int = 10000000) extends Component {

  val io = new Bundle {
    val screen_sck = out(RegInit(False))
    val screen_mosi = out(RegInit(False))
    val screen_rstn = out(RegInit(False))
    val screen_dc = out(RegInit(False))
    val screen_cs = out(RegInit(True))
  }
  noIoPrefix()

  // val image_index = Reg(UInt(log2Up(1024) bits)) init 0

  // 初始化列表，MSB=0表示命令，MSB=1表示数据
  val setup_cmds_1 = Cat(
    B"9'h0CF",
    B"9'h100",
    B"9'h1c9",
    B"9'h130",
    B"9'h0ed",
    B"9'h164",
    B"9'h103",
    B"9'h112",
    B"9'h181",
    B"9'h0e8",
    B"9'h185",
    B"9'h110",
    B"9'h17A",
    B"9'h0cb",
    B"9'h139",
    B"9'h12c",
    B"9'h100",
    B"9'h134",
    B"9'h102",
    B"9'h0f7",
    B"9'h120",
    B"9'h0ea",
    B"9'h100",
    B"9'h100",
    B"9'h0c0", // Power control
    B"9'h11b", // VRH[5:0]

    B"9'h0c1", // Power control
    B"9'h100", // SAP[2:0];BT[3:0] 01

    B"9'h0c5", // VCM control
    B"9'h130", // 3F
    B"9'h130", // 3C

    B"9'h0c7", // VCM control2
    B"9'h1b7",
    B"9'h036", // Memory Access Control
    B"9'h108",
    B"9'h03A", // Memory Access Control
    B"9'h155",
    B"9'h0b1",
    B"9'h100",
    B"9'h11A",
    B"9'h0B6", // Display Function Control
    B"9'h10A",
    B"9'h1A2",
    B"9'h0F2", // 3Gamma Function Disable
    B"9'h100",
    B"9'h026", // Gamma curve selected
    B"9'h101",
    B"9'h0E0", // Set Gamma
    B"9'h10F",
    B"9'h12A",
    B"9'h128",
    B"9'h108",
    B"9'h10E",
    B"9'h108",
    B"9'h154",
    B"9'h1A9",
    B"9'h143",
    B"9'h10A",
    B"9'h10F",
    B"9'h100",
    B"9'h100",
    B"9'h100",
    B"9'h100",
    B"9'h0E1", // Set Gamma
    B"9'h100",
    B"9'h115",
    B"9'h117",
    B"9'h107",
    B"9'h111",
    B"9'h106",
    B"9'h12B",
    B"9'h156",
    B"9'h13C",
    B"9'h105",
    B"9'h110",
    B"9'h10F",
    B"9'h13F",
    B"9'h13F",
    B"9'h10F",
    B"9'h02B",
    B"9'h100",
    B"9'h100",
    B"9'h101",
    B"9'h13f",
    B"9'h02A",
    B"9'h100",
    B"9'h100",
    B"9'h100",
    B"9'h1ef",
    B"9'h011" // Exit Sleep

    /*
     * LCD_direction(USE_HORIZONTAL);//设置LCD显示方向
      LCD_LED=1;//点亮背光
      LCD_Clear(WHITE);//清全屏白色*/
  )
  val init_cmds_idx_1 = RegInit(U(90))

  val setup_cmds_2 = Cat(
    // delay_ms(120)
    B"9'h029", // display on

    B"9'h036", // LCD_direction
    B"9'h108" //// BGR==1,MY==0,MX==0,MV==0
    // LC_CLEAR  待实现
  )
  val init_cmds_idx_2 = RegInit(U(3))

  val set_window_cmds = Cat(
    // lcddev.setxcmd = 0x2A
    // lcddev.setycmd = 0x2B
    // lcddev.wramcmd = 0x2C
    /*
	LCD_WriteRAM_Prepare//开始写入GRAM
     * */

    B"9'h02A", // setxcmd
    B"9'h100", // xStar:0
    B"9'h100", // 0
    B"9'h100", // xEnd: 240 >> 8
    B"9'h1f0", // 240 & 0xff

    B"9'h02B", // setycmd
    B"9'h100", // yStar:0
    B"9'h100", // 0
    B"9'h101", // yEnd: 320 >> 8
    B"9'h140", // 320 & 0xff

    B"9'h02C" // LCD_WriteRAM_Prepare//开始写入GRAM
  )
  val set_window_cmds_index = RegInit(U(11))

  val pixel_cnt = Counter(240 * 320)

  val counter = Counter(reset_cnt * 4)
  object State extends SpinalEnum {
    val start_up, init_cmds_1, delay_2, init_cmds_2, send_bits, check_finished, send_pixel = newElement()
  }
  val state = RegInit(State.start_up)

  val spi_dr = Reg(Bits(16 bits)) init 0
  val bits_to_cnt = RegInit(U(15))
  val spi_clk_div_cnt = Counter(2)
  switch(state) {
    is(State.start_up) {
      counter.increment()
      when(counter.value < reset_cnt) {
        io.screen_rstn := True
      } elsewhen (counter.value < reset_cnt * 2) {
        io.screen_rstn := False
      } elsewhen (counter.value < reset_cnt * 3) {
        io.screen_rstn := True
      } otherwise {
        state := State.init_cmds_1
        counter.clear()
      }
    }

    is(State.delay_2) {
      counter.increment()
      when(counter.value > reset_cnt) {
        state := State.init_cmds_2
        counter.clear()
      }
    }

    is(State.init_cmds_1) {
      io.screen_dc := setup_cmds_1((init_cmds_idx_1 * 9 - 1).resized) // MSB为命令/数据
      io.screen_cs := False
      spi_dr := B"00000000" ## setup_cmds_1((init_cmds_idx_1 - 1) * 9, 8 bits)
      init_cmds_idx_1 := init_cmds_idx_1 - 1
      state := State.send_bits
      bits_to_cnt := 7
      spi_clk_div_cnt.clear()
    }

    is(State.init_cmds_2) {
      io.screen_dc := setup_cmds_2((init_cmds_idx_2 * 9 - 1).resized) // MSB为命令/数据
      io.screen_cs := False
      spi_dr := B"00000000" ## setup_cmds_2((init_cmds_idx_2 - 1) * 9, 8 bits)
      init_cmds_idx_2 := init_cmds_idx_2 - 1
      state := State.send_bits
      bits_to_cnt := 7
      spi_clk_div_cnt.clear()
    }

    is(State.send_bits) {
      spi_clk_div_cnt.increment()
      when(spi_clk_div_cnt.value === 0) {
        io.screen_sck := False
        io.screen_mosi := spi_dr(bits_to_cnt)
      }
      when(spi_clk_div_cnt.value === 1) {
        io.screen_sck := True
        bits_to_cnt := bits_to_cnt - 1
        when(bits_to_cnt === 0) {
          state := State.check_finished
        }
      }
    }
    is(State.check_finished) {
      io.screen_cs := True
      when(init_cmds_idx_1 > 0) {
        state := State.init_cmds_1
      } elsewhen (init_cmds_idx_2 === 3) {
        state := State.delay_2
      } elsewhen (init_cmds_idx_2 > 0) {
        state := State.init_cmds_2
      } otherwise {
        state := State.send_pixel
      }
    }
    is(State.send_pixel) {
      when(set_window_cmds_index > 0) {
        io.screen_dc := set_window_cmds((set_window_cmds_index * 9 - 1).resized) // MSB为命令/数据
        io.screen_cs := False
        spi_dr := B"00000000" ## set_window_cmds((set_window_cmds_index - 1) * 9, 8 bits)
        set_window_cmds_index := set_window_cmds_index - 1
        state := State.send_bits
        bits_to_cnt := 7
        spi_clk_div_cnt.clear()
      } otherwise {
        pixel_cnt.increment()
        io.screen_dc := True // MSB为命令/数据
        io.screen_cs := False

        when(pixel_cnt < 240 * 80) {
          spi_dr := B(0xffe0)
        } elsewhen (pixel_cnt < 240 * 160) {
          spi_dr := B(0xf800)
        } elsewhen (pixel_cnt < 240 * 240) {
          spi_dr := B(0x001f)
        } otherwise {
          spi_dr := B(0x7fff)
        }

        state := State.send_bits
        bits_to_cnt := 15
        spi_clk_div_cnt.clear()
        when(pixel_cnt.willOverflow) {
          set_window_cmds_index := 11
        }
      }
    }
  }
}

object screen_ili9341 extends App {
  Config.spinal.generateVerilog(new screen_ili9341())
}
object screen_ili9341Sim extends App {
  Config.sim.compile(new screen_ili9341(reset_cnt = 100)).doSim { dut =>
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
