// Camp cleanup
import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    var line = StdIn.readLine()
    var sum1: Int = 0
    var sum2: Int = 0
    while (line!=null) {
      // split and parse each pair of ranges
      val pair = line.split(',')
      if (pair.length!=2) {
        println("oops: line not a pair?: "+line)
        return
      }
      val elf1 = pair(0).split('-')
      val elf2 = pair(1).split('-')
      if (elf1.length!=2 || elf2.length!=2) {
        println("oops: not a range?: "+elf1+","+elf2)
        return
      }
      val r1b = elf1(0).toInt
      val r1e = elf1(1).toInt
      val r2b = elf2(0).toInt
      val r2e = elf2(1).toInt
      // containment check!
      if (r1b<=r2b && r1e>=r2e)
        sum1 += 1
      else if (r2b<=r1b && r2e>=r1e)
        sum1 += 1
      // overlap check (part2)
      if (!(r1e<r2b || r2e<r1b))
        sum2 += 1
      line = StdIn.readLine()
    }
    println("Sum1: "+sum1+", Sum2: "+sum2)
  }
}
