// Monkey Map - maze solving..
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {

/* cube mapping...

choose between test/real based on height: h < 20 => test else real

test:
        +----------+
        |  +----+  |
        |  |    v  v
        |  |+-->1111<-----+
        |  ||   1111      |
        |  ||   1111      |
        v  vv  +1111<----+|
   +--->222233334444<--+ ||
   |    222233334444   | ||
   |    222233334444   | ||
   |  +>222233334444+  v ||
   |  | ^  ^^  +55556666<+|
   |  | |  ||   55556666  |
   |  | |  ||   55556666  |
   |  | |  |+-->55556666<-+
   |  | |  |    ^  ^^  ^
   |  | |  +----+  ||  |
   |  | +----------+|  |
   |  +-------------+  |
   +-------------------+

    face determination:
      (w/2 <= x < 3w/4) && (0 <= y < h/3) => face1
      (0 <= x < w/4) && (h/3 <= y < 2h/3) => face2
      (w/4 <= x < w/2) && (h/3 <= y < 2h/3) => face3
      (w/2 <= x < 3w/4) && (h/3 <= y < 2h/3) => face4
      (w/2 <= x < 3w/4) && (2h/3 <= y < h) => face5
      (3w/h <= x < w) && (2h/3 <= y < h) => face6

    portals:
      face1,RIGHT => face6,LEFT,Y=>-Y
      face1,LEFT  => face3,DOWN,Y=>X
      face1,UP    => face2,DOWN,X=>-X
      face2,DOWN  => face5,UP,X=>-X
      face2,LEFT  => face6,UP,Y=>-X
      face2,UP    => face1,DOWN,X=>-X
      face3,DOWN  => face5,RIGHT,X=>-Y
      face3,UP    => face1,RIGHT,X=>Y
      face4,RIGHT => face6,DOWN,Y=>-X
      face5,DOWN  => face2,UP,X=>-X
      face5,LEFT  => face3,UP,Y=>-X
      face6,RIGHT => face1,LEFT,Y=>-Y
      face6,DOWN  => face2,RIGHT,X=>-Y
      face6,UP    => face4,LEFT,X=>-Y

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

    face determination:
      (w/3 <= x < 2w/3) && (0 <= y < h/4) => face1
      (2w/3 <= x < w) && (0 <= y < h/4) => face2
      (w/3 <= x < 2w/3) && (h/4 <= y < h/2) => face3
      (0 <= x < w/3) && (h/2 <= y < 3h/4) => face4
      (w/3 <= x < 2w/3) && (h/2 <= y < 3h/4) => face5
      (0 <= x < w/3) && (3h/4 <= y < h) => face6

    portals:
      face1,LEFT  => face4,RIGHT,Y=>-Y
      face1,UP    => face6,RIGHT,X=>Y
      face2,RIGHT => face5,LEFT,Y=>-Y
      face2,DOWN  => face3,LEFT,X=>Y
      face2,UP    => face6,UP,X=>X
      face3,RIGHT => face2,UP,Y=>X
      face3,LEFT  => face4,DOWN,Y=>X
      face4,LEFT  => face1,RIGHT,Y=>-Y
      face4,UP    => face3,RIGHT,X=>Y
      face5,RIGHT => face2,LEFT,Y=>-Y
      face5,DOWN  => face6,LEFT,X=>Y
      face6,RIGHT => face5,UP,Y=>X
      face6,DOWN  => face2,DOWN,X=>X
      face6,LEFT  => face1,DOWN,Y=>X

 */

  val FACE1 = 0
  val FACE2 = 1
  val FACE3 = 2
  val FACE4 = 3
  val FACE5 = 4
  val FACE6 = 5
  def xytoface(c: (Int,Int), p: (Int,Int)): Int = {
    val (w,h) = c
    val (x,y) = p
    if (h < 20) { // Test cube
      if (y < h/3)
        FACE1
      else if (y < (2*h)/3 && x < w/4)
        FACE2
      else if (y < (2*h)/3 && x < w/2)
        FACE3
      else if (y < (2*h)/3 && x < (3*w)/4)
        FACE4
      else if (x < (3*w)/4)
        FACE5
      else
        FACE6
    } else {    // Real cube
      if (y < h/4 && x < (2*w)/3)
        FACE1
      else if (y < h/4)
        FACE2
      else if (y < h/2)
        FACE3
      else if (y < (3*h)/4 && x < w/3)
        FACE4
      else if (y < (3*h)/4)
        FACE5
      else
        FACE6
    }
  }
  def facetoxywh(c: (Int,Int), f: Int): (Int,Int,Int,Int) = {
    val (w,h) = c
    if (c._2 < 20) {  // Test cube
      f match {
        case FACE1 => (w/2, 0, w/4, h/3)
        case FACE2 => (0, h/3, w/4, h/3)
        case FACE3 => (w/4, h/3, w/4, h/3)
        case FACE4 => (w/2, h/3, w/4, h/3)
        case FACE5 => (w/2, (2*h)/3, w/4, h/3)
        case FACE6 => ((3*w)/4, (2*h)/3, w/4, h/3)
        case _ => sys.error(f.toString)
      }
    } else {          // Real cube
      f match {
        case FACE1 => (w/3, 0, w/3, h/4)
        case FACE2 => ((2*w)/3, 0, w/3, h/4)
        case FACE3 => (w/3, h/4, w/3, h/4)
        case FACE4 => (0, h/2, w/3, h/4)
        case FACE5 => (w/3, h/2, w/3, h/4)
        case FACE6 => (0, (3*h)/4, w/3, h/4)
      }
    }
  }
  val RIGHT= 0
  val DOWN = 1
  val LEFT = 2
  val UP   = 3
  val NONE = 0
  val INVR = 1
  val tports = Map(
    (FACE1,RIGHT) -> (FACE6,LEFT,INVR),
    (FACE1,LEFT)  -> (FACE3,DOWN,NONE),
    (FACE1,UP)    -> (FACE2,DOWN,INVR),
    (FACE2,DOWN)  -> (FACE5,UP,INVR),
    (FACE2,LEFT)  -> (FACE6,UP,INVR),
    (FACE2,UP)    -> (FACE1,DOWN,INVR),
    (FACE3,DOWN)  -> (FACE5,RIGHT,INVR),
    (FACE3,UP)    -> (FACE1,RIGHT,NONE),
    (FACE4,RIGHT) -> (FACE6,DOWN,INVR),
    (FACE5,DOWN)  -> (FACE2,UP,INVR),
    (FACE5,LEFT)  -> (FACE3,UP,INVR),
    (FACE6,RIGHT) -> (FACE1,LEFT,INVR),
    (FACE6,DOWN)  -> (FACE2,RIGHT,INVR),
    (FACE6,UP)    -> (FACE4,LEFT,INVR)
  )
  val rports = Map(
    (FACE1,LEFT)  -> (FACE4,RIGHT,INVR),
    (FACE1,UP)    -> (FACE6,RIGHT,NONE),
    (FACE2,RIGHT) -> (FACE5,LEFT,INVR),
    (FACE2,DOWN)  -> (FACE3,LEFT,NONE),
    (FACE2,UP)    -> (FACE6,UP,NONE),
    (FACE3,RIGHT) -> (FACE2,UP,NONE),
    (FACE3,LEFT)  -> (FACE4,DOWN,NONE),
    (FACE4,LEFT)  -> (FACE1,RIGHT,INVR),
    (FACE4,UP)    -> (FACE3,RIGHT,NONE),
    (FACE5,RIGHT) -> (FACE2,LEFT,INVR),
    (FACE5,DOWN)  -> (FACE6,LEFT,NONE),
    (FACE6,RIGHT) -> (FACE5,UP,NONE),
    (FACE6,DOWN)  -> (FACE2,DOWN,NONE),
    (FACE6,LEFT)  -> (FACE1,DOWN,NONE)
  )
  def cubewrap(c: (Int,Int), pos: (Int,Int,Int)): (Int,Int,Int) = {
    val (x,y,d) = pos
    // determine starting face..
    val f = xytoface(c, (x,y))
    // map through portal..
    val (nf,nd,tr) = if (c._2 < 20) tports((f,d)) else rports((f,d))
    // obtain co-ords for current and new faces..
    val fg = facetoxywh(c,f)
    val (fx,fy,fw,fh) = fg
    val ng = facetoxywh(c,nf)
    val (nfx,nfy,_,_) = ng
    // calculate current offset along edge of interest
    val of = d match {
      case RIGHT|LEFT => y - fy
      case UP|DOWN => x - fx
      case _ => sys.error(d.toString)
    }
    // translate offset along new edge..
    val (nx,ny) = (nd,tr) match {
      case (RIGHT,NONE)  => (nfx,nfy+of)
      case (RIGHT,INVR)  => (nfx,nfy+fh-1-of)
      case (LEFT,NONE)   => (nfx+fw-1,nfy+of)
      case (LEFT,INVR)   => (nfx+fw-1,nfy+fh-1-of)
      case (DOWN,NONE)   => (nfx+of,nfy)
      case (DOWN,INVR)   => (nfx+fw-1-of,nfy)
      case (UP,NONE)     => (nfx+of,nfy+fh-1)
      case (UP,INVR)     => (nfx+fw-1-of,nfy+fh-1)
      case _ => sys.error(f"invalid new dir/trans: ${nd}/${tr}")
    }
    //print(f"op=${pos}/f=${f+1}/m=${(nf+1,nd,tr)}/fg=${fg}/ng=${ng}/np=${(nx,ny)}")
    (nx,ny,nd)
  }
  def cubemove(map: ArrayBuffer[ArrayBuffer[Char]], c: (Int,Int), pos: (Int,Int,Int)): (Int,Int,Int) = {
    val (x,y,d) = pos
    val (nx,ny,nd) = d match {
      case RIGHT => if ((x+1) >= map(y).length || map(y)(x+1)==' ') cubewrap(c,pos) else (x+1,y,d)
      case DOWN  => if ((y+1) >= map.length || x >= map(y+1).length || map(y+1)(x)==' ') cubewrap(c,pos) else (x,y+1,d)
      case LEFT  => if ((x<1) || map(y)(x-1)==' ') cubewrap(c,pos) else (x-1,y,d)
      case UP    => if ((y<1) || x >= map(y-1).length || map(y-1)(x)==' ') cubewrap(c,pos) else (x,y-1,d)
      case _ => sys.error(d.toString)
    }
    if (map(ny)(nx)!='.') pos else (nx,ny,nd)
  }
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
  def main(args: Array[String]): Unit = {
    val map: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer()
    var inMap = true
    var maxw = 0
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
        maxw = if (row.length > maxw) row.length else maxw
      } else {
        // https://www.baeldung.com/java-split-string-keep-delimiters
        inst = line.split("((?=[RL])|(?<=[RL]))")
      }
      line = StdIn.readLine()
    }
    println(f"maxw=${maxw} h=${map.length}")
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
    // part2: follow them again, but round a cube..
    x = 0
    y = 0
    d = RIGHT
    while (map(y)(x)==' ')
      x += 1
    println(f"pos=${d}@(${x},${y})")
    for (ins <- inst) {
      ins match {
        case "R" => d = (d+1)%4
        case "L" => d = (d+3)%4
        case _ => {
          var s = ins.toInt
          while (s>0) {
            val np = cubemove(map,(maxw,map.length),(x,y,d))
            x = np._1
            y = np._2
            d = np._3
            //print(f"pos=${d}@(${x},${y}) ")
            s -= 1
          }
          //println()
        }
      }
    }
    val part2 = 1000*(y+1) + 4*(x+1) + d
    println(f"part2: ${part2}")
  }
}
