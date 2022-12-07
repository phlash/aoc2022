// Supply stack - aka towers of Hanoi :)
import scala.io.StdIn
import scala.collection.mutable.Stack

object Main {
  def main(args: Array[String]): Unit = {
    // We keep a stack for each tower/stack
    // in an list of stacks.
    var stacks: List[Stack[Char]] = List()
    var line = StdIn.readLine()
    // first, parse the stack diagram..in reverse :)
    var rev: Stack[String] = Stack()
    while (line != null && line.length>0) {
      rev.push(line)
      line = StdIn.readLine()
    }
    // create stacks..
    line = rev.pop()
    for (s <- line.trim().split("\\s+")) {
      stacks = stacks :+ Stack()
    }
    println("Stacks:"+stacks.length)
    // populate them..
    while (rev.length>0) {
      line = rev.pop()
      var s: Int = 0
      while (s < line.length) {
        if (line.charAt(s) == '[') {
          val c = line.charAt(s+1)
          stacks(s/4).push(c)
        }
        s += 4
      }
    }

    // now parse actions, and shuffle crates
    line = StdIn.readLine()
    while (line != null) {
      val t = line.trim().split("\\s+")
      val c = t(1).toInt
      val s = t(3).toInt
      val d = t(5).toInt
      for (m <- 1 to c) {
        stacks(d-1).push(stacks(s-1).pop())
      }
      line = StdIn.readLine()
    }
    // print top of stacks
    for (s <- stacks) {
      print(s.pop())
    }
    println()
  }
}
