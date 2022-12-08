// No Space Left - oh good, a command history parser..
import scala.io.StdIn

// all directory data is held in this recursive type
class Dirent(val name:String, val size:Int, val parent:Dirent) {
  var items:Vector[Dirent] = Vector()
  def add(n:String, s:Int = 0): Unit = {
    items = items :+ new Dirent(n, s, this)
  }
  def get(n:String): Dirent = {
    // special ".." term
    if (".." == n) return parent
    // special "/" term
    if ("/" == n) {
      if (parent != null)
        return parent.get("/")
      else
        return this
    }
    for (i <- items) {
      if (i.name == n) return i
    }
    return null
  }
  def dump(pfx:String): Unit = {
    println(pfx+"name:"+name+", size:"+size)
    for (i <- items)
      i.dump(pfx+"  ")
  }
}

object Main {
  var acc: Int = 0
  var low: Int = 30000000
  def size(d: Dirent): Int = {
    var s = 0
    for (i <- d.items)
      s += size(i)
    s += d.size
    if (d.items.length>0 && s<100000) {
      acc += s
      println("acc: "+d.name+"="+s+" => "+acc)
    }
    return s
  }
  def lowest(d: Dirent, r:Int): Int = {
    var s = 0
    for (i <- d.items)
      s += lowest(i, r)
    s += d.size
    if (d.items.length>0 && s>=r && s<low) {
      low = s;
      println("Lowest: "+d.name+"="+low)
    }
    return s
  }
  def main(args: Array[String]): Unit = {
    val root = new Dirent("/", 0, null)
    var line = StdIn.readLine()
    var cwd = root
    while (line != null) {
      // tokenize and parse the line..
      val toks = line.trim().split("\\s+")
      toks(0) match {
        case "$" => {
          toks(1) match {
            case "cd" => cwd = cwd.get(toks(2))
            case "ls" => {}
            case _ => println("Unknown cmd: "+toks(1))
          }
        }
        case "dir" => cwd.add(toks(1))
        case _ => cwd.add(toks(1), toks(0).toInt)
      }
      line = StdIn.readLine()
    }
    // print the tree
    root.dump("")
    val tot = size(root)
    println("Total: "+tot)
    val req = 30000000-(70000000-tot)
    println("Req: "+req)
    lowest(root, req)
  }
}
