// Grove Positioning System - aka cipher cracking?
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    var din: ArrayBuffer[(Int,Int)] = ArrayBuffer()
    var line = StdIn.readLine()
    var pos = 0
    while (line != null) {
      din = din :+ (pos,line.toInt)
      pos += 1
      line = StdIn.readLine()
    }
    println(f"input: ${din}")
    var dout = din.clone
    for (p <- din) {
      // find it..
      var pos = dout.indexOf(p)
      assert(pos>=0 && pos<dout.length)
      // determine step count & direction..
      var stp = p._2.abs
      //print(f"stp=${stp} ")
      if (stp!=0) {
        val dir = p._2/stp
        //print(f"dir=${dir} ")
        // move value..
        while (stp>0) {
          var npos = pos+dir
          if (npos<0)
            npos = dout.length-1
          if (npos>dout.length-1)
            npos = 0
          //print(f"${pos}=>${npos} ")
          // swap values..
          val t = dout(npos)
          dout(npos) = dout(pos)
          dout(pos) = t
          stp -= 1
          pos = npos
        }
      }
      //println(f": ${dout}")
    }
    // calculate co-ordinates
    val zpos = dout.indexWhere(_._2==0)
    println(f"zero@${zpos}")
    val part1 =
      dout((zpos+1000)%dout.length)._2 +
      dout((zpos+2000)%dout.length)._2 +
      dout((zpos+3000)%dout.length)._2
    println(f"part1: ${part1}")
  }
}
