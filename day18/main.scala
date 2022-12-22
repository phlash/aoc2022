// Boiling Boulders - cuboid geometry fun..
import scala.io.StdIn
import scala.collection.mutable.HashMap

object Main {
  val EXT = 0
  val VIS = 1
  val CUBE = 2
  var minX = Int.MaxValue
  var maxX = Int.MinValue
  var minY = Int.MaxValue
  var maxY = Int.MinValue
  var minZ = Int.MaxValue
  var maxZ = Int.MinValue
  def pathOut(dep:Int, scan: HashMap[(Int,Int,Int),Int], xyz: (Int,Int,Int)): Boolean = {
    // if we reach an existing cell, return as appropriate
    //print(f"[${xyz}@${dep}:")
    if (scan.contains(xyz)) {
      //print(f"${scan(xyz)}]")
      return scan(xyz) match {
        case EXT => true
        case _   => false
      }
    }
    // if we reach an edge, we're EXT
    val (x,y,z) = xyz
    if (x<=minX || x>=maxX || y<=minY || y>=maxY || z<=minZ || z>=maxZ) {
      //print("E]")
      scan(xyz) = EXT
      return true
    }
    // search adjoining volume cells
    // if we find an EXT, we are also EXT
    scan(xyz) = VIS
    for (dxyz <- List( (-1,0,0),(1,0,0),(0,-1,0),(0,1,0),(0,0,-1),(0,0,1) )) {
      val (dx,dy,dz) = dxyz
      val nxt = (x+dx,y+dy,z+dz)
      if (pathOut(dep+1, scan, nxt)) {
        //print("e]")
        scan(xyz) = EXT
        return true
      }
    }
    // no way out, try other paths..
    //print("?]")
    return false
  }
  def interiorFaces(scan: HashMap[(Int,Int,Int),Int], xyz: (Int,Int,Int)): Int = {
    print(f"iF${xyz}: ")
    // if we are a CUBE, then ignore
    if (scan.contains(xyz) && scan(xyz)==CUBE) {
      println("CUBE")
      return 0
    }
    // clear all the visited markers..
    for (x <- minX to maxX)
      for (y <- minY to maxY)
        for (z <- minZ to maxZ)
          if (scan.contains((x,y,z)) && scan((x,y,z))==VIS)
            scan -= ((x,y,z))
    // search for a path out of the space, if found we are EXT, ignore
    if (pathOut(0, scan, xyz)) {
      println("=EXT")
      return 0
    }
    // now we have tried all routes out, we are confident this is INT
    print("=INT/")
    // count faces that adjoin CUBEs
    var count = 0
    val (x,y,z) = xyz
    for (dxyz <- List( (-1,0,0),(1,0,0),(0,-1,0),(0,1,0),(0,0,-1),(0,0,1) )) {
      val (dx,dy,dz) = dxyz
      val nxt = (x+dx,y+dy,z+dz)
      if (scan.contains(nxt) && scan(nxt)==CUBE)
        count += 1
    }
    println(count)
    return count
  }
  def main(args: Array[String]): Unit = {
    var scan: HashMap[(Int,Int,Int),Int] = HashMap()
    var line = StdIn.readLine()
    while (line != null) {
      val xyz = line.split(",").map(_.toInt)
      val x = xyz(0)
      val y = xyz(1)
      val z = xyz(2)
      scan((x,y,z)) = CUBE
      line = StdIn.readLine()
    }
    // count exposed cube faces.. assume 6, remove adjoining faces by testing
    var total = 0
    for (c <- scan) {
      var faces = 6
      val (x,y,z) = c._1
      minX = if (x<minX) x else minX
      minY = if (y<minY) y else minY
      minZ = if (z<minZ) z else minZ
      maxX = if (x>maxX) x else maxX
      maxY = if (y>maxY) y else maxY
      maxZ = if (z>maxZ) z else maxZ
      for (dx <- List(-1,1))
        if (scan.contains((x+dx,y,z)))
          faces -= 1
      for (dy <- List(-1,1))
        if (scan.contains((x,y+dy,z)))
          faces -= 1
      for (dz <- List(-1,1))
        if (scan.contains((x,y,z+dz)))
          faces -= 1
      total += faces
    }
    println(f"part1: ${total}")
    // now consider interior spaces, and remove those from the count..
    // iterate volume, subtract any interior faces
    for (x <- minX to maxX)
      for (y <- minY to maxY)
        for (z <- minZ to maxZ)
          total -= interiorFaces(scan, (x,y,z))
    println(f"part2: ${total}")
  }
}
