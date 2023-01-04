// Unstable Diffusion - yet more map fun!
import scala.io.StdIn
import scala.collection.mutable.HashMap

class Elf(var pos: (Int,Int), var active: Boolean) {
  override def toString = f"Elf@${pos}:${active}"
}

object Main {
  val border = List((-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1))
  val movexy = HashMap(
    'N' -> List((-1,-1),(0,-1),(1,-1)),
    'S' -> List((-1,1),(0,1),(1,1)),
    'W' -> List((-1,-1),(-1,0),(-1,1)),
    'E' -> List((1,-1),(1,0),(1,1))
  )
  def main(args: Array[String]): Unit = {
    val map: HashMap[(Int,Int),Elf] = HashMap()
    var line = StdIn.readLine()
    var y = 0
    while (line != null) {
      var x = 0
      for (c <- line) {
        c match {
          case '#' => map((x,y)) = new Elf((x,y), true)
          case _ => {}
        }
        x += 1
      }
      line = StdIn.readLine()
      y += 1
    }
    // 10 rounds of Elf movement according to obtuse rules
    var moves = List('N','S','W','E')
    var minx = Int.MaxValue
    var maxx = Int.MinValue
    var miny = Int.MaxValue
    var maxy = Int.MinValue
    var round = 0
    var nmvs = 1
    while (nmvs>0) {
      round += 1
      println(f"round: ${round}")
      // first, deactivate any isolated Elves, or gather proposed moves
      val props: HashMap[(Int,Int),List[Elf]] = HashMap()
      for (e <- map) {
        val (pos,elf) = e
        val (ex,ey) = elf.pos
        elf.active = border.exists(xy => map.contains(ex+xy._1,ey+xy._2))
        if (elf.active) {
          val prop = moves.find(
            m => !movexy(m).exists(
              xy => map.contains(ex+xy._1,ey+xy._2)
            )
          ) match {
            case Some('N') => Option(ex,ey-1)
            case Some('S') => Option(ex,ey+1)
            case Some('W') => Option(ex-1,ey)
            case Some('E') => Option(ex+1,ey)
            case _ => None
          }
          for (pv <- prop)
            if (props.contains(pv))
              props(pv) = props(pv) :+ elf
            else
              props(pv) = List(elf)
        }
      }
      // second, execute moves if no collisions
      nmvs = 0
      for (prop <- props) {
        val (npos,elfs) = prop
        if (elfs.length < 2) {
          val elf = elfs(0)
          map(npos) = map.remove(elf.pos).getOrElse(sys.error("removing elf"))
          elf.pos = npos
          nmvs += 1
        }
      }
      // find containing co-ords
      for (e <- map) {
        val (pos,elf) = e
        val (ex,ey) = pos
        minx = if (ex<minx) ex else minx
        miny = if (ey<miny) ey else miny
        maxx = if (ex>maxx) ex else maxx
        maxy = if (ey>maxy) ey else maxy
      }
      println(f"moves=${moves} moved=${nmvs} container=(${minx},${miny})->(${maxx},${maxy})")
      // finally, rotate the move order
      moves = moves.slice(1,4) ++ moves.slice(0,1)
      if (10==round) {
        // part1: free ground is container size - elf count @ round 10
        val part1 = (maxx-minx+1)*(maxy-miny+1) - map.size
        println(f"part1: ${part1}")
      }
    }
    // part2: what round do we stop moving?
    println(f"part2: ${round}")
  }
}
