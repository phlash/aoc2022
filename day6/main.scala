// Tuning trouble - pattern detection..
import scala.io.StdIn

object Main {
  def start(history: String): Boolean = {
    println("chk:"+history)
    for (i <- 0 to history.length-2)
      for (j <- i+1 to history.length-1)
        if (history.charAt(i)==history.charAt(j))
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
    println("Part1:"+(pos+4))
    pos = 0
    history = line.substring(pos,pos+14)
    while (!start(history)) {
      pos += 1
      history = line.substring(pos,pos+14)
    }
    println("Part2:"+(pos+14))
  }
}
