// Elephants, Caves, tunnels, valves... a graph solver.
import scala.io.StdIn
import scala.collection.mutable.{HashMap,Queue}

class GraphNode(val bitmask: Long, val name:String, val flow:Int, val tunnels:List[String]) {
  override def toString = f"GraphNode(${name}($bitmask%#x),${flow},${tunnels})"
}

object Main {

  // Depth-first-search (DFS) to find maximum pressure release in <n> iterations
  // ..also populates a map of paths indexed by a bitmask of nodes
  def maxPressure(loaded: List[GraphNode], distmap: HashMap[(String,String),Int], visited: List[String], start: String, iter: Int, pres: Long, bitpaths: HashMap[Long,Long] = HashMap()): Long = {
    //println(f"${start}:iter=${iter} pres=${pres} path=${visited}")
    if (0 == iter) {
      println("- oops")
      return pres
    }
    val node = loaded.find(n => n.name == start).get
    // can we open a valve here?
    val (opres,oiter) = if (node.flow > 0 && iter > 1) {
      ((node.flow*(iter-1)).toLong, iter-1)
    } else {
      (0L, iter)
    }
    // update bitpaths
    val epres = pres + opres
    val nvisited = visited :+ start
    // nb: we exclude the starting node, otherwise all bitmasks contain the same bit
    // which messes up the distinct test later..
    val bitmask = nvisited.tail.map(name => loaded.find(n => n.name==name).get.bitmask).sum
    bitpaths(bitmask) = if (bitpaths.contains(bitmask)) { if (epres > bitpaths(bitmask)) epres else bitpaths(bitmask) } else epres
    // can we go elsewhere?
    var mpres = epres
    distmap.filter(e => {
      // all routes from here..
      e._1._1==start &&
      // ..that we haven't been to..
      !visited.contains(e._1._2) &&
      // ..that we have time to..
      oiter > e._2
    }).foreach(e => {
      val vpres = maxPressure(loaded, distmap, nvisited, e._1._2, oiter-e._2, epres, bitpaths)
      mpres = if (vpres>mpres) vpres else mpres
    })
    // backtrack state
    //println(f"${start}:iter=${iter} mpres=${mpres}")
    mpres
  }

  // BFS to find shortest distance from start node to all other nodes
  def bfs(loaded: List[GraphNode], distmap: HashMap[(String,String),Int], queue: Queue[(String,Int)], start: String) {
    if (queue.length>0) {
      // process one queue entry..
      val node = queue.dequeue()
      // look up next node
      val next = loaded.find(n => n.name == node._1).get
      // add to distance map if not already there (BFS guarantees shortest route found first)
      if (start!=next.name && !distmap.contains((start,next.name)))
        distmap((start,next.name)) = node._2
      // add tunnels from here to queue, plus one to distance
      next.tunnels.foreach(name => {
        if (!distmap.contains((start,name))) queue.enqueue((name, node._2+1))
      })
      // tail recurse to next entry
      bfs(loaded, distmap, queue, start)
    }
  }
  def main(args: Array[String]): Unit = {
    var loaded:List[GraphNode] = List()
    var line = StdIn.readLine()
    var bitmask = 1L
    while (line != null) {
      // parse line to obtain name, flow and tunnels
      var o = line.indexOf(" ")
      var e = if (o>0) line.indexOf(" ", o+1) else -1
      val n = if (e>0) line.substring(o+1,e) else ""
      o = if (e>0) line.indexOf("=", e) else -1
      e = if (o>0) line.indexOf(";", o) else -1
      val f = if (e>0) line.substring(o+1,e).toInt else -1
      o = if (e>0) line.indexOf(",",e) else -1
      o = if (o<0) line.length else o
      val t = if (o>0) line.substring(o-2).split(",").map(_.trim) else Array[String]()
      val node = if (n.length>0 && f != -1) new GraphNode(bitmask, n, f, t.toList) else null
      loaded = loaded :+ node
      bitmask <<= 1
      line = StdIn.readLine()
    }
    println(f"loaded:\n${loaded}")
    // reduce loaded data to a graph of minimum distances between all nodes
    // using a breadth-first-search (BFS)
    val tempmap = HashMap[(String,String),Int]()
    for (node <- loaded) {
      val queue = Queue[(String,Int)]()
      node.tunnels.foreach(name => queue.enqueue((name,1)))
      bfs(loaded, tempmap, queue, node.name)
    }
    println(f"tempmap size:${tempmap.size}")
    // prune zero-flow (useless) nodes from the distance map, except "AA"!
    val distmap = tempmap.filter(e => {
      val n1 = loaded.find(n => n.name==e._1._1).get
      val n2 = loaded.find(n => n.name==e._1._2).get
      n1.name=="AA" || (n1.flow > 0 && n2.flow > 0)
    })
    println(f"distmap size:${distmap.size}")
    // part1 - find the highest pressure in 30 steps
    val mpres = maxPressure(loaded, distmap, List(), "AA", 30, 0)
    println(f"part1: ${mpres}")
    // part2 - find two distinct paths that have the highest combined pressure over 26 steps
    val bitpaths = HashMap[Long,Long]()
    maxPressure(loaded, distmap, List(), "AA", 26, 0, bitpaths)
    println(f"bitpaths size:${bitpaths.size}")
    var part2 = 0L
    var cnt = 0
    for {
      p1 <- bitpaths
      p2 <- bitpaths
      if ((p1._1 & p2._1)==0)
    } {
      part2 = if ((p1._2+p2._2) > part2) (p1._2+p2._2) else part2
      cnt += 1
    }
    println(f"part2: cnt=${cnt} ${part2}")
  }
}
