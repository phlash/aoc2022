// Rope Bridge..
import scala.io.StdIn
import scala.collection.mutable.Map

object Main {
  def move(pos:(Int,Int,Int,Int), m:(Int,Int)): (Int,Int,Int,Int) = {
    var (hx,hy,tx,ty) = pos
    var (x,y) = m
    hx += x
    hy += y
    // tail movement rules..
    val dx = hx-tx
    val dy = hy-ty
    // if directly aligned but 2+ steps away, move toward
    if (0==dy) {
      if (dx < -1) tx-=1
      if (dx > 1) tx+=1
    } else if (0==dx) {
      if (dy < -1) ty-=1
      if (dy > 1) ty+=1
    // diagonally aligned, if 2+ steps away, move toward diagonally
    } else if (dx>1) {
      tx+=1
      if (dy>0) ty+=1
      if (dy<0) ty-=1
    } else if (dx < -1) {
      tx-=1
      if (dy>0) ty+=1
      if (dy<0) ty-=1
    } else if (dy > 1) {
      ty+=1
      if (dx>0) tx+=1
      if (dx<0) tx-=1
    } else if (dy < -1) {
      ty-=1
      if (dx>0) tx+=1
      if (dx<0) tx-=1
    }
    return (hx,hy,tx,ty)
  }
  def main(args: Array[String]): Unit = {
    // starting position of head & tail
    var pos = (0,0,0,0)
    // coverage map
    val cvg: collection.mutable.Map[(Int,Int),Int] = Map()
    cvg += ((0,0) -> 1)
    var line = StdIn.readLine()
    while (line != null) {
      val d = line.charAt(0)
      val l = line.substring(2).toInt
      val m = d match {
        case 'L' => (-1,0)
        case 'R' => (1,0)
        case 'U' => (0,1)
        case 'D' => (0,-1)
        case _ => { println("oops, unknown direction:"+d); (0,0) }
      }
      for (i <- 1 to l) {
        pos = move(pos,m)
        val (hx,hy,tx,ty) = pos
        cvg += ((tx,ty) -> 1)
        println(m+">"+pos)
      }
      line = StdIn.readLine()
    }
    for (p <- cvg)
      println(p)
    println("Final count:"+(cvg.size))
  }
}
