// Blizard Basin - a moving map solver!
import scala.io.{StdIn,Source}
import scala.collection.mutable.{ArrayBuffer,Queue,HashMap}

object Main {
  val WALL = -1
  val FREE = 0
  val LEFT = 1
  val RIGHT= 2
  val UP   = 4
  val DOWN = 8
  val hex  = Array("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F")
  def dump(map: ArrayBuffer[ArrayBuffer[Int]], pos: (Int,Int) = (-1,-1), end: (Int,Int) = (-1,-1), visited: HashMap[(Int,Int,Int),Int] = HashMap(), step: Int = -1) {
    var y  = 0
    for (r <- map) {
      var x = 0
      for (v <- r) {
        print(v match {
          case WALL => "#"
          case FREE => if ((x,y) == pos) "E" else if ((x,y) == end) "X" else if (visited.contains((x,y,step))) "o" else "."
          case LEFT => "<"
          case RIGHT=> ">"
          case UP   => "^"
          case DOWN => "v"
          case _ => hex(v)
        })
        x += 1
      }
      println()
      y += 1
    }
    println()
  }
  def move(map: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
    val next = map.map(_.map(v => if (v==WALL) WALL else FREE))
    var y = 0
    for (r <- map) {
      var x = 0
      for (v <- r) {
        if (v!=WALL) {
          if ((v & LEFT)!=0) {
            val nx = if (x>1) x-1 else map(y).length-2
            next(y)(nx) = next(y)(nx) | LEFT
          }
          if ((v & RIGHT)!=0) {
            val nx = if (x<map(y).length-2) x+1 else 1
            next(y)(nx) = next(y)(nx) | RIGHT
          }
          if ((v & UP)!=0) {
            val ny = if (y>1) y-1 else map.length-2
            next(ny)(x) = next(ny)(x) | UP
          }
          if ((v & DOWN)!=0) {
            val ny = if (y<map.length-2) y+1 else 1
            next(ny)(x) = next(ny)(x) | DOWN
          }
        }
        x += 1
      }
      y += 1
    }
    next
  }
  // @see https://www.artima.com/pins1ed/functional-objects.html
  def gcd(a:Int, b:Int): Int =
    if (b==0) a else gcd(b, a%b)

  def search(maps: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]], start: (Int,Int), end: (Int,Int), istp: Int): (Int,Int) = {
    // operate a breadth-first search to find the shortest route out..
    val maxx = maps(0)(0).length
    val maxy = maps(0).length
    val moves = List((0,0),(-1,0),(1,0),(0,-1),(0,1))
    val queue = Queue[(Int,Int)]()
    // visited state includes map cycle position, to account for moving map
    var visited = HashMap[(Int,Int,Int),Int]()
    var step = istp
    var dist = 0
    var done = false
    queue.enqueue(start)
    while (!done) {
      val ns = (step+1)%maps.length
      val dq = queue.dequeueAll(p => true)
      assert(queue.length==0)
      // mark as visted, all the cells we are about to visit..
      for (nxt <- dq)
        visited((nxt._1,nxt._2,step)) = dist
      // ..then visit each cell
      for (nxt <- dq) {
        if (done) {}  // are we done?
        else if (nxt == end) {
          done = true
        } else {      // where can we go next?
          val (x,y) = nxt
          queue.enqueue(moves.map(p => (p._1+x,p._2+y)).filter(p => {
            // valid position?
            p._1 >= 0 && p._1 < maxx && p._2 >= 0 && p._2 < maxy && maps(ns)(p._2)(p._1)==FREE &&
            // have we been before?
            !visited.contains(p._1,p._2,ns) &&
            // don't repeat ourselves in this step
            !queue.contains((p._1,p._2))
          }):_*)
        }
      }
      print(f"\r${dist}   ")
      Console.flush()
      //dump(maps(step), start, end, visited, step)
      //println(f"at dist=${dist},step=${step},queue=${queue},visited=${visited}")
      if (!done) {
        step = ns
        dist += 1
      }
      /*
      val tty = Source.fromFile("/dev/tty")
      try {
        tty.getLines().next()
      } finally {
        tty.close()
      }
      */
    }
    (step,dist)
  }
  def main(args: Array[String]): Unit = {
    val maps = ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]()
    var line = StdIn.readLine()
    var map = ArrayBuffer[ArrayBuffer[Int]]()
    while (line != null) {
      val row = ArrayBuffer[Int]()
      for (c <- line)
        row.append(c match {
          case '#' => WALL
          case '.' => FREE
          case '<' => LEFT
          case '>' => RIGHT
          case '^' => UP
          case 'v' => DOWN
          case _ => sys.error(c.toString)
        })
      map.append(row)
      line = StdIn.readLine()
    }
    val pos = (map(0).indexOf(FREE),0)
    val end = (map(map.length-1).indexOf(FREE),map.length-1)
    val wid = map(0).length-2
    val hgt = map.length-2
    // @see https://en.wikipedia.org/wiki/Least_common_multiple
    val lcm = (wid*hgt).abs/gcd(wid,hgt)
    println(f"lcm=${lcm}")
    maps.append(map)
    for (i <- 1 until lcm) {
      map = move(map)
      maps.append(map)
    }
    println(f"maps.length=${maps.length}")
    println(f"pos=${pos} end=${end}")
    val (s1,part1) = search(maps, pos, end, 0)
    println(f"part1: ${part1}")
    val (s2,d2) = search(maps, end, pos, s1)
    println(f"intr: ${d2}")
    val (s3,d3) = search(maps, pos, end, s2)
    val part2 = part1+d2+d3
    println(f"part2: ${part2}")
  }
}
