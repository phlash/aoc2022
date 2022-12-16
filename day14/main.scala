// Falling sand in a grid...
import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {
  // grid cell contents, one of:
  val AIR = 0
  val ROCK = 1
  val SAND = 2
  def ensureGrid(grid:ArrayBuffer[ArrayBuffer[Int]], x:Int, y:Int): ArrayBuffer[ArrayBuffer[Int]] = {
    // early exit check
    if (grid.length>y && grid(0).length>x)
      return grid
    var ng:ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer()
    // copy & extend any existing rows..
    for (row <- grid) {
      var nr: ArrayBuffer[Int] = ArrayBuffer() ++ row
      while (nr.length<=x)
        nr = nr :+ AIR
      ng = ng :+ nr
    }
    // append any extra rows
    while (ng.length<=y) {
      var nr: ArrayBuffer[Int] = new ArrayBuffer()
      val l = if (ng.length>0) ng(0).length else x
      while (nr.length<=l)
        nr = nr :+ AIR
      ng = ng :+ nr
    }
    return ng
  }

  def pour(grid:ArrayBuffer[ArrayBuffer[Int]], floor:Boolean=false): Boolean = {
    // starting at 500,0, trace a sand grain through the grid until it stops or falls away
    var x = 500
    var y = 0
    // when we have a floor, termination is by filling up to the starting point
    if (floor && grid(y)(x)==SAND)
      return false
    while (y<grid.length-1) {
      // if we can move down, do it
      if (grid(y+1)(x)==AIR) {
        y += 1
      // if we can move down & left, do it
      } else if (x>0 && grid(y+1)(x-1)==AIR) {
        y += 1
        x -= 1
      // if we can move down & right, do it
      } else if (x<grid(0).length-1 && grid(y+1)(x+1)==AIR) {
        y += 1
        x += 1
      // otherwise we stop here
      } else {
        grid(y)(x) = SAND
        println("=stop")
        return true
      }
      print((x,y))
    }
    // hit the floor
    if (floor) {
      grid(y)(x) = SAND
      println("=floor")
      return true
    }
    // fell out the bottom
    println("=fall")
    return false
  }

  def clear(grid:ArrayBuffer[ArrayBuffer[Int]]):Unit =
    for (r <- grid)
      for (c <- 0 to r.length-1)
        if (r(c)==SAND) r(c) = AIR

  def main(args: Array[String]): Unit = {
    var grid: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer()
    var line = StdIn.readLine()
    while (line != null) {
      // create the grid
      var pairs: Vector[(Int,Int)] = Vector()
      for (p <- line.split("->").map(_.trim)) {
        val xy = p.split(",").map(_.toInt)
        grid = ensureGrid(grid, xy(0), xy(1))
        pairs = pairs :+ (xy(0),xy(1))
      }
      // expand path segments in each line into the grid
      // (not happy with the verbosity here, but range loops blow up when start==end?)
      var i = 0
      while (i < pairs.length-1) {
        var (x1,y1) = pairs(i)
        var (x2,y2) = pairs(i+1)
        if (x1==x2) {
          if (y2<y1) {
            val t = y2
            y2 = y1
            y1 = t
          }
          var y = y1
          while (y <= y2) {
            grid(y)(x1) = ROCK
            y += 1
          }
        } else if (y1==y2) {
          if (x2<x1) {
            val t = x2
            x2 = x1
            x1 = t
          }
          var x = x1
          while (x <= x2) {
            grid(y1)(x) = ROCK
            x += 1
          }
        } else {
          println("diagonal line!")
          return
        }
        i += 1
      }
      line = StdIn.readLine()
    }
    println(f"grid ${grid(0).length}x${grid.length}")

    // pour in the sand, until it falls out the bottom
    var sand = 0
    while (pour(grid))
      sand += 1
    println("part1: "+sand)
    // clear the sand, try again with a floor at grid.length+2 until we fill up
    clear(grid)
    grid = ensureGrid(grid,grid(0).length*2,grid.length)
    sand = 0
    while (pour(grid,true))
      sand += 1
    println("part2: "+sand)
  }
}
