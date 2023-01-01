// Grove Positioning System - aka cipher cracking?
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]): Unit = {
    var din: ArrayBuffer[(Long,Long)] = ArrayBuffer()
    var line = StdIn.readLine()
    var pos = 0
    val key = if (args.length>0) 811589153L else 1L
    val loop = if (args.length>0) 10 else 1
    while (line != null) {
      din = din :+ (pos.toLong,line.toLong*key)
      pos += 1
      line = StdIn.readLine()
    }
    println(f"input: ${din}")
    var dout = din.clone
    val dlen = dout.length
    for (i <- 0 until loop) {
      println(i)
      for (p <- din) {
        // find it..
        var pos = dout.indexOf(p)
        assert(pos>=0)
        // determine step count (modulo array size!) & direction..
        var stp = p._2.abs % (dlen-1)
        //print(f"stp=${p._2}/${stp} ")
        if (stp!=0) {
          val dir = if (p._2>0) 1 else -1
          //print(f"dir=${dir} ")
          // move value..
          while (stp>0) {
            var npos = pos+dir
            val tst = (pos,npos)
            // special case, slide everything left, append.
            if (tst == (1,0)) {
                dout = (dout(0) +: dout.slice(2, dlen)) :+ p
                npos = dlen-1
            }
            // special case, slide most left, insert penultimate pos.
            else if (tst == (0,-1)) {
              dout = (dout.slice(1, dlen-1) :+ p) :+ dout(dlen-1)
              npos = dlen-2
            }
            // special case, slide everything right, prepend.
            else if (tst == (dlen-2,dlen-1)) {
              dout = p +: (dout.slice(0,dlen-2) :+ dout(dlen-1))
              npos = 0
            }
            // special case, slide most right, insert pos 1
            else if (tst == (dlen-1,dlen)) {
              dout = dout(0) +: (p +: dout.slice(1,dlen-1))
              npos = 1
            }
            // standard case, swap
            else {
              dout(pos) = dout(npos)
              dout(npos) = p
            }
            //println(f"stp=${stp} p=${p} ${dout}")
            assert(dlen==dout.length)
            stp -= 1
            pos = npos
          }
        }
        //println(f": ${dout}")
      }
    }
    // calculate co-ordinates
    val zpos = dout.indexWhere(_._2==0)
    println(f"zero@${zpos}")
    val res =
      dout((zpos+1000)%dout.length)._2 +
      dout((zpos+2000)%dout.length)._2 +
      dout((zpos+3000)%dout.length)._2
    println(f"result: ${res}")
  }
}
