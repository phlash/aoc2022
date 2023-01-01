// Monkey Math - a solver tree..
import scala.io.StdIn
import scala.collection.mutable.HashMap

object Main {
  def doop(op: String, lhs: ()=>Long, rhs: ()=>Long): Long =
    op match {
      case "+" => lhs() + rhs()
      case "-" => lhs() - rhs()
      case "*" => lhs() * rhs()
      case "/" => lhs() / rhs()
      case _ => { assert(false); 0L }
    }

  def main(args: Array[String]): Unit = {
    // We use a map, from monkey name to function returning value
    val monkeys: HashMap[String,() => Long] = HashMap()
    var line = StdIn.readLine()
    while (line != null) {
      // either:
      // <monkey>: <value> => function returns value
      // or:
      // <monkey>: <name> <op> <name> => function applies op to other map entries
      val toks = line.split(" ")
      val name = toks(0).substring(0,toks(0).length-1)
      toks.length match {
        case 2 => monkeys(name) = () => toks(1).toLong
        case 4 => monkeys(name) = () => doop(toks(2), monkeys(toks(1)), monkeys(toks(3)))
        case _ => assert(false)
      }
      line = StdIn.readLine()
    }
    val part1 = monkeys("root")()
    println(f"part1: ${part1}")
  }
}
