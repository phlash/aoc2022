// Rope Bridge..
import scala.io.StdIn
import scala.collection.mutable.{Map, ArrayBuffer}

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
    // starting position of rope nodes
    var rope: ArrayBuffer[(Int,Int)] = ArrayBuffer()
    rope = rope.padTo(10, (0,0))
    println(rope)
    // coverage maps (node 1 and 9: parts 1 & 2)
    val part1: collection.mutable.Map[(Int,Int),Int] = Map()
    val part2: collection.mutable.Map[(Int,Int),Int] = Map()
    part1 += ((0,0) -> 1)
    part2 += ((0,0) -> 1)
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
        // move the rope, one node at a time
        for (i <- 0 to 8) {
          val (hx,hy,tx,ty) = move(
            (rope(i)._1,rope(i)._2,rope(i+1)._1,rope(i+1)._2),
            if (i>0) (0,0) else m)
          rope = rope.patch((i+1),List((tx,ty)),1)
          if (0==i) {
            rope = rope.patch(i,List((hx,hy)),1)
            part1 += ((tx,ty) -> 1)
          }
          if (8==i)
            part2 += ((tx,ty) -> 1)
        }
        println(m+">"+rope)
      }
      line = StdIn.readLine()
    }
    for (p <- part1)
      println(p)
    println("Part1 count:"+(part1.size))
    for (p <- part2)
      println(p)
    println("Part2 count:"+(part2.size))
  }
}
