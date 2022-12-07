// Tuning trouble - pattern detection..
import scala.io.StdIn

object Main {
  def start(history: String): Boolean = {
    println("chk:"+history)
    if (history.charAt(0) == history.charAt(1) ||
        history.charAt(0) == history.charAt(2) ||
        history.charAt(0) == history.charAt(3) ||
        history.charAt(1) == history.charAt(2) ||
        history.charAt(1) == history.charAt(3) ||
        history.charAt(2) == history.charAt(3))
      return false
    return true
  }

  def main(args: Array[String]): Unit = {
    var line = StdIn.readLine()
    var pos: Int = 0
    var history = line.substring(pos,pos+4)
    while (!start(history)) {
      pos += 1
      history = line.substring(pos,pos+4)
    }
    println("Found:"+(pos+4))
  }
}
