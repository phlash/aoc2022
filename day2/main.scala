// Playing RPS and calculating a score..
import scala.io.StdIn

object Main {
  def round(them: Int, you: Int): Int = {
    println("Round:"+them+","+you)
    var score: Int = you
    if (you==them) {
      score += 3
    } else if (you==them+1 || (you==1 && them==3)) {
      score += 6
    }
    return score
  }

  def choose(them: Int, result: Int): Int = {
    println("Choose:"+them+","+result)
    result match {
      case 1 => { if (them>1) { them-1 } else { 3 } }
      case 2 => them
      case 3 => { if (them<3) { them+1 } else { 1 } }
      case _ => -1
    }
  }

  def main(args: Array[String]): Unit = {
    var line = StdIn.readLine()
    var score1: Int = 0
    var score2: Int = 0
    while (line != null) {
      val abc = line(0)
      val xyz = line(2)
      val them = abc match {
        case 'A' => 1
        case 'B' => 2
        case 'C' => 3
        case _ => -1
      }
      val you = xyz match {
        case 'X' => 1
        case 'Y' => 2
        case 'Z' => 3
        case _ => -1
      }
      if (them<0 || you<0) {
        println("Oops, failed to parse:"+line)
        return
      }
      score1 += round(them, you)
      score2 += round(them, choose(them, you))
      line = StdIn.readLine()
    }
    println("Score1:"+score1)
    println("Score2:"+score2)
  }
}
