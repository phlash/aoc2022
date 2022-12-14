// Hill Climbing, aka shortest path algorithm..
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

class Cell(val height:Int, var cost:Int = Int.MaxValue) {}

object Main {
  def shortest(map:ArrayBuffer[ArrayBuffer[Cell]], pos:(Int,Int), top:(Int,Int), path:List[(Int,Int)]): Int = {
    // the rules
    // - can ONLY move vertically/horiontally
    // - can ONLY ascend one unit
    // - can descend
    // Part2 magic - we search in reverse, as then start point is constant

    val (x,y) = pos
    if ((-1,-1)!=top && pos==top) { // Yay, we made it!
      println(pos+":top@"+path.length)
      return path.length
    }
    if ((-1,-1)==top && 0==map(y)(x).height) { // dynaimic termination for part2 - any 'a' will do
      println(pos+"a@"+path.length)
      return path.length
    }
    // update lowest cost to this cell
    map(y)(x).cost = path.length
    // filter & sort possible directions in steepness
    val hgt = map(y)(x).height
    val dirs = List((x-1,y),(x+1,y),(x,y-1),(x,y+1)).filter(
        (d) => {
          val (tx,ty) = d
          val o = tx>=0 && tx<map(0).length && ty>=0 && ty<map.length
          val p = o && !path.contains(d)
          val h = p && map(ty)(tx).height>hgt-2
          val c = h && map(ty)(tx).cost>path.length+1 // Djikstra magic - avoid more expensive options
          c
        }
      ).sortWith(
        (a,b) => {
          val (ax,ay) = a
          val (bx,by) = b
          val ah = map(ay)(ax).height
          val bh = map(by)(bx).height
          ah<bh
        }
      )
    // recursive search..
    var d = -1
    for (nxt <- dirs) {
      var td = shortest(map, nxt, top, path :+ pos)
      if (d<0 || (td>=0 && td<d))
        d = td
    }
    return d
  }

  def main(args: Array[String]): Unit = {
    var map: ArrayBuffer[ArrayBuffer[Cell]] = ArrayBuffer()
    var line = StdIn.readLine()
    var width = 0
    var x = 0
    var y = 0
    var start = (0,0)
    var top = (0,0)
    while (line != null) {
      if (width<1) {
        width = line.length
      } else if (width != line.length) {
        println("oops: map width changed?")
        return
      }
      var row:ArrayBuffer[Cell] = new ArrayBuffer(width)
      x = 0
      for (c <- line) {
        c match {
          case 'S' => { start = (x,y); row = row :+ new Cell(0) }
          case 'E' => { top = (x,y); row = row :+ new Cell(25) }
          case _ =>   row = row :+ new Cell(c-'a')
        }
        x += 1
      }
      map = map :+ row
      y += 1
      line = StdIn.readLine()
    }
    println(f"map(${width}x${y}) start=${start} top=${top}")

    // Part1: Go find that route!
    var short = shortest(map, top, start, List())
    println("part1: shortest path="+short)
    // Part2: go find shortest starting point
    for (r <- map)
      for (c <- r)
        c.cost = Int.MaxValue
    short = shortest(map, top, (-1,-1), List())
    println("part2: shortest start="+short)
  }
}
