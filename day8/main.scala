// Treetop Treehouse..
import scala.io.StdIn

object Main {
  def isvisible(x:Int, y:Int, trees:Vector[IndexedSeq[Int]]): Boolean = {
    // try all routes to an edge, stop if any route is visible
    val h = trees(y)(x)
    val my = trees.length
    val mx = trees(0).length
    var r = true
    for (px <- 0 to x-1)
      if (trees(y)(px) >= h)
        r = false
    if (r)
      return r
    r = true
    for (px <- x+1 to mx-1)
      if (trees(y)(px) >= h)
        r = false
    if (r)
      return r
    r = true
    for (py <- 0 to y-1)
      if (trees(py)(x) >= h)
        r = false
    if (r)
      return r
    r = true
    for (py <- y+1 to my-1)
      if (trees(py)(x) >= h)
        r = false
    return r
  }
  def main(args: Array[String]): Unit = {
    var trees: Vector[IndexedSeq[Int]] = Vector()
    var line = StdIn.readLine()
    var size: Int = 0
    while (line != null) {
      if (size<1) {
        size = line.length
        println("size:"+size)
      }
      var b = line.map(x => (x-'0').toInt)
      trees = trees :+ b
      line = StdIn.readLine()
    }
    // dump it..
    for (r <- trees) {
      for (v <- r)
        print(v+",")
      println()
    }
    // part1: count visible trees
    var part1 = (size-1)*2 + (trees.length-1)*2
    for (y <- 1 to trees.length-2)
      for (x <- 1 to size-2)
        if (isvisible(x, y, trees))
          part1 += 1
    println("Visible: "+part1)
  }
}
