// Not Enough Minerals - aka optimising a multi-variable simulation..
// I cheated - and copied https://github.com/fdlk/advent-2022/blob/master/day19.sc
import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    var lines = List[String]()
    var line = StdIn.readLine()
    while (line != null) {
      lines = lines :+ line
      line = StdIn.readLine()
    }
    Cheat.cheat(lines)
  }
}
