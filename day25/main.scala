// Full of Hot Air - weird encoding parsers
import scala.io.StdIn

object Main {
  // nb: does not convert negative values!
  def dec2snafu(dec: Long): String = {
    var rem = dec
    var res = ""
    while (rem>0) {
      res = "012=-"((rem%5).toInt) + res
      rem = (rem+2)/5
    }
    res
  }
  def snafu2dec(line: String): Long = {
    var tot = 0L
    for (c <- line) {
      tot *= 5
      tot += (c match {
        case '2' => 2
        case '1' => 1
        case '0' => 0
        case '-' => -1
        case '=' => -2
        case _ => sys.error(c.toString)
      })
    }
    tot
  }
  def main(args: Array[String]): Unit = {
    var line = StdIn.readLine()
    var tot = 0L
    while (line != null) {
      val nxt = snafu2dec(line)
      println(nxt)
      tot += nxt
      line = StdIn.readLine()
    }
    println(f"tot:${tot}")
    println(f"part1:${dec2snafu(tot)}")
  }
}
