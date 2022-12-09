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
  def scenic(x:Int, y:Int, trees:Vector[IndexedSeq[Int]]): Int = {
    // count number of visible trees in all directions..
    val h = trees(y)(x)
    val my = trees.length
    val mx = trees(0).length
    var px = x
    var sl = 0
    while (px>0) {
      sl += 1
      px -= 1
      if (trees(y)(px)>=h)
        px = 0
    }
    var sr = 0
    px = x
    while (px<mx-1) {
      sr += 1
      px += 1
      if (trees(y)(px)>=h)
        px = mx
    }
    var py = y
    var su = 0
    while (py>0) {
      su += 1
      py -= 1
      if (trees(py)(x)>=h)
        py = 0
    }
    var sd = 0
    py = y
    while (py<my-1) {
      sd += 1
      py += 1
      if (trees(py)(x)>=h)
        py = my
    }
    return sl*sr*su*sd
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
    // part1: count visible trees
    var part1 = (size-1)*2 + (trees.length-1)*2
    for (y <- 1 to trees.length-2)
      for (x <- 1 to size-2)
        if (isvisible(x, y, trees))
          part1 += 1
    println("Visible: "+part1)
    // part2: find tree with highest scenic score
    var ms = 0
    var mx = 0
    var my = 0
    for (y <- 1 to trees.length-2)
      for (x <- 1 to size-2) {
        var s = scenic(x, y, trees)
        if (s>ms) {
          ms = s
          mx = x
          my = y
        }
      }
    println("Max scenic: "+ms+" @ "+mx+","+my)
  }
}
