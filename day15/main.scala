// Calorie counting - simple line-by-line input, parsing integers and adding up..
import scala.io.StdIn

object Main {
  def mhd(a:(Int,Int), b:(Int,Int)):Int =
    (a._1-b._1).abs + (a._2-b._2).abs

  def main(args: Array[String]): Unit = {
    var line = StdIn.readLine()
    var scan: List[(Int,Int)] = List()
    var sens: List[(Int,Int,Int)] = List()
    var bcns: List[(Int,Int)] = List()
    // part1: scanning @ y=10 (test) or y=2000000 (real)
    val ty = if (args.length>0) 2000000 else 10
    while (line != null) {
      // parse pairs of sensor/beacon positions
      var o = line.indexOf("x=")
      var e = if (o>0) line.indexOf(", y=",o) else -1
      var sx = if (e>0) line.substring(o+2,e).toInt else -1
      o = if (e>0) e else -1
      e = if (o>0) line.indexOf(":",o) else -1
      var sy = if (e>0) line.substring(o+4,e).toInt else -1
      o = if (e>0) line.indexOf("x=",e) else -1
      e = if (o>0) line.indexOf(", y=",o) else -1
      var bx = if (e>0) line.substring(o+2,e).toInt else -1
      o = if (e>0) e else -1
      var by = if (o>0) line.substring(o+4).toInt else -1
      bcns = bcns :+ (bx,by)
      val md = mhd((sx,sy),(bx,by))
      sens = sens :+ (sx,sy,md)
      println(f"s=(${sx},${sy}) b=(${bx},${by}) mhd=${md}")
      // the calculation is:
      // work out remaining manhattan distance, if <1 ignore this sensor
      // apply remaining distance both ways along line of interest from sensor
      val rem = (md-(sy-ty).abs)
      if (rem>=0) {
        scan = scan :+ (sx-rem,sx+rem)
      }
      line = StdIn.readLine()
    }
    // combine scans to count positions that cannot contain beacon
    println(scan)
    var minx = scan(0)._1
    var maxx = scan(0)._2
    for (s <- scan) {
      if (s._1 < minx)
        minx = s._1
      if (s._2 > maxx)
        maxx = s._2
    }
    println(f"check ${minx}-${maxx}")
    var count = 0
    for (v <- minx to maxx) {
      var ok = !bcns.contains((v,ty))
      for (s <- scan) {
        if (ok && v >= s._1 && v <= s._2) {
          count += 1
          ok = false
        }
      }
    }
    println("part1: "+count)
    // part2: try a more efficient way to locate a gap in a scan line..
    val len = if(args.length>0) 4000000 else 20
    var p2b = new Array[Int](len)
    for (o <- 0 to len/2) {
      for (y <- List(len/2-o, len/2+o)) {
      if ((y%100)==0)
        println(y)
      for (x <- 0 until len)
        p2b(x) = 0
      for (sen <- sens) {
        val (sx,sy,md) = sen
        val rem = (md-(sy-y).abs)
        if (rem>=0) {
          var x = sx-rem
          while (x<=sx+rem) {
            if (x>=0 && x<len)
              p2b(x) = 1
            x += 1
          }
        }
      }
      for (x <- 0 until len) {
        if (p2b(x)==0) {
          println(f" part2: (${x},${y})=${x*4000000+y}")
          return
        }
      }
    }
    }
  }
}
