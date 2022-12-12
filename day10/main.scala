// Cathode-Ray Tube (or weird CPU emulator?)
import scala.io.StdIn

object Main {
  def check(time:Int, xreg:Int, sum:Int): Int =
      if (time==20 || ((time-20)%40)==0) {
        println("time:"+time+", x:"+xreg+", ss:"+(time*xreg))
        sum + time*xreg
      } else {
        sum
      }

  def main(args: Array[String]): Unit = {
    var xreg = 1
    var time = 0
    var part1 = 0
    var line = StdIn.readLine()
    while (line != null) {
      if (line.startsWith("noop")) {
        time += 1
        part1 = check(time, xreg, part1)
      } else {
        time += 1
        part1 = check(time, xreg, part1)
        time += 1
        part1 = check(time, xreg, part1)
        xreg += line.split(" ")(1).toInt
      }
      line = StdIn.readLine()
    }
    println("Part1:"+part1)
  }
}
