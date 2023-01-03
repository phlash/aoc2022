// Monkey Map - maze solving..
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

/* cube mapping...
test:
        +----------+
        |  +----+  |
        |  |    v  v
        |  |+-->1111<-----+
        |  ||   1111      |
        |  ||   1111      |
        v  vv   1111      |
   +--->222233334444<--+  |
   |    222233334444   |  |
   |    222233334444   |  |
   |  +>222233334444   v  |
   |  | ^  ^^   55556666  |
   |  | |  ||   55556666  |
   |  | |  ||   55556666  |
   |  | |  |+-->55556666<-+
   |  | |  |    ^  ^^  ^
   |  | |  +----+  ||  |
   |  | +----------+|  |
   |  +-------------+  |
   +-------------------+

real:
  +------------++--------+
  |+--------+  ||  +----+|
  ||        v  vv  v    ||
  || +----->11112222<-+ ||
  || |      11112222  | ||
  || |      11112222  | ||
  || |+---->11112222<+| ||
  || || +-->3333+  ^ || ||
  || || |   3333   | || ||
  || || |   3333   | || ||
  || || v  +3333<--+ || ||
  || |+>44445555<----+| ||
  || |  44445555      | ||
  || |  44445555      | ||
  || +->44445555<-----+ ||
  |+--->6666+  ^        ||
  |     6666   |        ||
  |     6666   |        ||
  +---->6666<--+        ||
        ^  ^            ||
        |  +------------+|
        +----------------+
 */

object Main {
  val RIGHT= 0
  val DOWN = 1
  val LEFT = 2
  val UP   = 3
  def move(map: ArrayBuffer[ArrayBuffer[Char]], pos: (Int,Int), dir: Int): (Int,Int) = {
    // the movement rules..
    val (x,y) = pos
    // wrapped single step
    val (nx,ny) = dir match {
      case RIGHT => if ((x+1) >= map(y).length || map(y)(x+1)==' ') (map(y).indexWhere(_!=' '),y) else (x+1,y)
      case DOWN  => {
        if ((y+1) >= map.length || x >= map(y+1).length || map(y+1)(x)==' ')
          (x,map.indexWhere(r => (x < r.length && r(x)!=' ')))
        else
          (x,y+1)
      }
      case LEFT  => if (x<1 || map(y)(x-1)==' ') (map(y).lastIndexWhere(_!=' '),y) else (x-1,y)
      case UP    => {
        if (y<1 || x >= map(y-1).length || map(y-1)(x)==' ')
          (x,map.lastIndexWhere(r => (x < r.length && r(x)!=' ')))
        else
          (x,y-1)
      }
      case _     => sys.error(dir.toString)
    }
    // collision detect
    if (map(ny)(nx)!='.') pos else (nx,ny)
  }
  def cubemove(map: ArrayBuffer[ArrayBuffer[Char]], pos: (Int,Int), dir: Int): (Int,Int) = {
    (0,0)
  }
  def main(args: Array[String]): Unit = {
    val map: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer()
    var inMap = true
    var inst: Array[String] = null
    var line = StdIn.readLine()
    while (line != null) {
      if (line.length<1)
        inMap = false
      else if (inMap) {
        val row = ArrayBuffer[Char]()
        for (c <- line)
          row.append(c)
        map.append(row)
      } else {
        // https://www.baeldung.com/java-split-string-keep-delimiters
        inst = line.split("((?=[RL])|(?<=[RL]))")
      }
      line = StdIn.readLine()
    }
    var x = 0
    var y = 0
    var d = RIGHT
    while (map(y)(x)==' ')
      x += 1
    println(f"pos=${d}@(${x},${y})")
    assert(map(y)(x)=='.')
    // part1: follow the instructions..
    for (ins <- inst) {
      ins match {
        case "R" => d = (d+1)%4
        case "L" => d = (d+3)%4
        case _ => {
          var s = ins.toInt
          while (s>0) {
            val np = move(map,(x,y),d)
            x = np._1
            y = np._2
            //print(f"pos=${d}@(${x},${y}) ")
            s -= 1
          }
          //println()
        }
      }
    }
    val part1 = 1000*(y+1) + 4*(x+1) + d
    println(f"part1: ${part1}")
  }
}
