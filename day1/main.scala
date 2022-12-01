// Calorie counting - simple line-by-line input, parsing integers and adding up..
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    var elfs = ArrayBuffer.empty[Int]
    elfs += 0
    var elf : Int = 0
    var line = StdIn.readLine()
    while (line != null) {
      if (line.length>0) {
        // value? add to current elf count
        elfs.update(elf, elfs(elf) + line.toInt)
      } else {
        // next elf
        elfs += 0
        elf += 1
      }
      line = StdIn.readLine()
    }
    println(elfs)
    val m1 = elfs.max
    println("Max:"+m1)
    elfs -= m1
    val m2 = elfs.max
    println("Next:"+m2)
    elfs -= m2
    val m3 = elfs.max
    println("Next:"+m3)
    println("Total:"+(m1+m2+m3))
  }
}
