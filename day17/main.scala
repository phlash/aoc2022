// Pyroclastic flow - or Tetris??
import scala.io.StdIn
import scala.collection.mutable.{ArrayBuffer, HashMap}

class Shape(val height:Int, val points:List[(Int,Int)]) {}

object Main {
  val rocks = List[Shape](
    new Shape(1, List((0,0), (1,0), (2,0), (3,0))),
    new Shape(3, List((1,0), (0,1), (1,1), (2,1), (1,2))),
    new Shape(3, List((2,0), (2,1), (0,2), (1,2), (2,2))),
    new Shape(4, List((0,0), (0,1), (0,2), (0,3))),
    new Shape(2, List((0,0), (1,0), (0,1), (1,1)))
  )
  val AIR:Int = 0
  val FLOOR:Int = 1
  val ROCK:Int = 2
  val DROP:Int = 3
  def dump(chamber:ArrayBuffer[ArrayBuffer[Int]]) = {
    for (r <- chamber) {
      print('|')
      for (c <- r)
        print(if (c==AIR) '.' else if (c==FLOOR) '-' else if (c==ROCK) '#' else '@')
      println('|')
    }
  }
  def canLeft(chamber:ArrayBuffer[ArrayBuffer[Int]], rock:Shape, x:Int, y:Int): Boolean = {
    // if we have ROCK to the left of any point..
    for (p <- rock.points)
      if (-1+x+p._1 < 0 || chamber(y+p._2)(-1+x+p._1) == ROCK)
        return false
    return true
  }
  def canRight(chamber:ArrayBuffer[ArrayBuffer[Int]], rock:Shape, x:Int, y:Int): Boolean = {
    // if we have ROCK to the right of any point..
    val w = chamber(0).length-1
    for (p <- rock.points)
      if (1+x+p._1 > w || chamber(y+p._2)(1+x+p._1) == ROCK)
        return false
    return true
  }
  def canFall(chamber:ArrayBuffer[ArrayBuffer[Int]], rock:Shape, x:Int, y:Int): Boolean = {
    // if we have ROCK below any point..
    var l = chamber.length-2
    for (p <- rock.points)
      if (1+y+p._2 > l || chamber(1+y+p._2)(x+p._1) == ROCK)
        return false
    return true
  }
  def drop(chamber:ArrayBuffer[ArrayBuffer[Int]], shape:Int, jets:String, jetP:Int, max:Int): (Int,Int) = {
    val rock = rocks(shape)
    var x = 2
    var y = 0
    var fall = true
    var j = jetP
    // put new rock in chamber
    for (p <- rock.points)
      chamber(y+p._2)(x+p._1) = DROP
    //println(f"new rock: x=${x}, y=${y}, j=${j}, jet:${jets(j)}")
    //dump(chamber)
    do {
      var nx = x
      var ny = y
      // alway apply gas jet
      if (jets(j)=='<' && canLeft(chamber, rock, nx, ny))
        nx -= 1
      else if (jets(j)=='>' && canRight(chamber, rock, nx, ny))
        nx += 1
      j = (j+1) % jets.length
      fall = canFall(chamber, rock, nx, ny)
      if (fall)
        ny += 1
      // redraw
      for (p <- rock.points)
        chamber(y+p._2)(x+p ._1) = AIR
      for (p <- rock.points)
        chamber(ny+p._2)(nx+p ._1) = if (fall) DROP else ROCK
      // move
      x = nx
      y = ny
      //println(f"x=${x}, y=${y}, j=${j}, jet:${jets(j)}, fall=${fall}")
      //dump(chamber)
    } while (fall)
    (j,if (chamber.length-y > max) chamber.length-y else max)
  }
  def main(args: Array[String]): Unit = {
    var jets = StdIn.readLine()
    var chamber = ArrayBuffer(
      ArrayBuffer(AIR, AIR, AIR, AIR, AIR, AIR, AIR),
      ArrayBuffer(AIR, AIR, AIR, AIR, AIR, AIR, AIR),
      ArrayBuffer(AIR, AIR, AIR, AIR, AIR, AIR, AIR),
      ArrayBuffer(AIR, AIR, AIR, AIR, AIR, AIR, AIR),
      ArrayBuffer(FLOOR, FLOOR, FLOOR, FLOOR, FLOOR, FLOOR, FLOOR)
    )
    var jetP = 0
    var top = 1
    var ldrp = 0
    var lhgt = top
    var pats: HashMap[(Int,Int,Int,Int),(Int,Int)] = HashMap()
    var limit = 100000
    var offset:Long = 0
    var r = 0
    while (r < limit) {
      // calculate height of chamber
      val ri = r%rocks.length
      val nh = top+3+rocks(ri).height
      // decrease chamber height if required..
      while (chamber.length>nh)
        chamber = chamber.drop(1)
      // increase chamber height if required for new rock
      while (chamber.length<nh)
        chamber = ArrayBuffer(AIR, AIR, AIR, AIR, AIR, AIR, AIR) +: chamber
      val (j,h) = drop(chamber, ri, jets, jetP, top)
      //println(f"after ${r} drop: j=${j}, h=${h}")
      // we've gone right round the jets array, look for a repeating height pattern
      if (offset==0 && j < jetP) {
        val dd = r - ldrp
        val dh = h - lhgt
        ldrp = r
        lhgt = h
        println(f"jet cycle@${h},j=${j},ri=${ri}: drop=${r}")
        // found a repeating pattern?
        val key = (j,ri,dd,dh)
        if (pats.contains(key)) {
          val lht:Long = h
          val ldr:Long = r
          val pdd:Long = r-pats(key)._1
          val pdh:Long = h-pats(key)._2
          // how many pattern repeats take us to within one pattern cycle of 10^12 drops?
          val prep:Long = (1000000000000L-ldr)/pdd
          // what's left to simulate?
          val prem:Long = 1000000000000L-ldr-(prep*pdd)
          limit = r + prem.toInt
          offset = (prep*pdh)
          println(f"pattern@${h} dd=${pdd} dh=${pdh}, reps=${prep} rem=${prem} offset=${offset}")
        }
        pats(key) = (r, h)
      }
      jetP = j
      top = h
      if (r==2021)
        println("part1:"+(top-1))
      r += 1
    }
    //dump(chamber)
    println("part2:"+(top.toLong-1L+offset))
  }
}
