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
    var part1: List[Stack[Char]] = stacks.map(x => x.clone())
    var part2: List[Stack[Char]] = stacks.map(x => x.clone())
    line = StdIn.readLine()
    while (line != null) {
      val t = line.trim().split("\\s+")
      val c = t(1).toInt
      val s = t(3).toInt
      val d = t(5).toInt
      var b: Stack[Char] = Stack()
      // part1 - move single crates..
      for (m <- 1 to c) {
        part1(d-1).push(part1(s-1).pop())
      }
      // part2 - move strings of crates..
      for (m <- 1 to c) {
        b.push(part2(s-1).pop())
      }
      for (m <- 1 to c) {
        part2(d-1).push(b.pop())
      }
      line = StdIn.readLine()
    }
    // print top of stacks
    print("Part1: ")
    for (s <- part1) {
      print(s.pop())
    }
    println()
    print("Part2: ")
    for (s <- part2) {
      print(s.pop())
    }
    println()
  }
}
