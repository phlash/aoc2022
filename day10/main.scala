// Cathode-Ray Tube (or weird CPU emulator?)
import scala.io.StdIn

object Main {
  def check(time:Int, xreg:Int, sum:Int): Int =
      if (time==20 || ((time-20)%40)==0) {
        //println("time:"+time+", x:"+xreg+", ss:"+(time*xreg))
        sum + time*xreg
      } else {
        sum
      }

  def render(time:Int, xreg:Int): Unit = {
    val pix = time%40
    if (pix>xreg-2 && pix<xreg+2)
      print('#')
    else
      print('.')
    if (39==pix)
      println()
  }

  def main(args: Array[String]): Unit = {
    var xreg = 1
    var time = 0
    var part1 = 0
    var line = StdIn.readLine()
    while (line != null) {
      if (line.startsWith("noop")) {
        render(time, xreg)
        time += 1
        part1 = check(time, xreg, part1)
      } else {
        render(time, xreg)
        time += 1
        part1 = check(time, xreg, part1)
        render(time, xreg)
        time += 1
        part1 = check(time, xreg, part1)
        xreg += line.split(" ")(1).toInt
      }
      line = StdIn.readLine()
    }
    println("Part1:"+part1)
  }
}
