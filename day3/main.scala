// Rucksack rearrangements..
import scala.io.StdIn

object Main {
  def find(s1: String, s2: String) =
    for (c1 <- s1;
         c2 <- s2 if c1 == c2)
      yield c1

  def badge(s1: String, s2: String, s3: String) =
    for (c1 <- s1;
         c2 <- s2;
         c3 <- s3 if c1 == c2 && c2 == c3)
      yield c1

  def xlat(c1: Char): Int = {
    if (c1>='a' && c1<='z') {
      c1-'a'+1
    } else {
      c1-'A'+27
    }
  }

  def main(args:Array[String]):Unit = {
    var sum1: Int = 0
    var sum2: Int = 0
    var line = StdIn.readLine()
    var group: List[String] = List()
    while (line!=null) {
      // convolve both halves of the rucksack to find matching char
      val s1 = line.substring(0, line.length/2)
      val s2 = line.substring(line.length/2)
      val c1 = find(s1, s2)(0)
      sum1 += xlat(c1)
      // add to group, find badge every three lines
      group = group :+ line
      if (group.length>2) {
        val b1 = badge(group(0), group(1), group(2))(0)
        group = List()
        sum2 += xlat(b1)
      }
      line = StdIn.readLine()
    }
    println("Sum1:"+sum1+", Sum2:"+sum2)
  }
}
