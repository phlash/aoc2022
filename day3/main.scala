// Rucksack rearrangements..
import scala.io.StdIn

object Main {
  def find(s1: String, s2: String) =
    for (c1 <- s1;
         c2 <- s2 if c1 == c2)
      yield c1

  def main(args:Array[String]):Unit = {
    var sum: Int = 0
    var line = StdIn.readLine()
    while (line!=null) {
      // convolve both halves of the rucksack to find matching char
      val s1 = line.substring(0, line.length/2)
      val s2 = line.substring(line.length/2)
      val c1 = find(s1, s2)(0)
      //println("s1="+s1+", s2="+s2+", c1="+c1)
      // translate matching char to priority and sum
      if (c1>='a' && c1<='z') {
        sum += c1-'a'+1
      } else {
        sum += c1-'A'+27
      }
      //println("sum:"+sum)
      line = StdIn.readLine()
    }
    println("Sum:"+sum)
  }
}
