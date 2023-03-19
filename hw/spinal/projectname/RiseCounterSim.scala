package projectname

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random
object RiseCounterSim {
  def main(args: Array[String]) {
    // 建立仿真 其中RiseCounter 来创建代码
    SimConfig.withWave.doSim(new RiseCounter) { dut =>
      // 创建时钟
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      // 注意 io的电平赋值用 #=
      dut.io.clear #= true
      // 等待三个时钟周期，不知道是否有更好的写法
      for(idx <- 0 to 3){
        dut.clockDomain.waitRisingEdge()
      }
      dut.io.clear #= false
      for(idx <- 0 to 99){
        //Drive the dut inputs with random values
        dut.io.sigIn #= Random.nextBoolean()
        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}