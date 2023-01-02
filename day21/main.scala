// Monkey Math - a solver tree..
import scala.io.StdIn
import scala.collection.mutable.HashMap

class Monkey(val name: String, val lhs: Monkey, val rhs: Monkey, val op: String) {
  var ish = false
  override def toString = "M("+op+")"+ (if (lhs!=null) "L"+lhs.toString) + (if (rhs!=null) "R"+rhs.toString)
  def find(): Boolean = {
    ish = name == "humn" ||
      (lhs!=null && lhs.find()) ||
      (rhs!=null && rhs.find())
    ish
  }
  def doop(): Long = {
    op match {
      case "+" => lhs.doop() + rhs.doop()
      case "-" => lhs.doop() - rhs.doop()
      case "*" => lhs.doop() * rhs.doop()
      case "/" => lhs.doop() / rhs.doop()
      case _ => op.toLong
    }
  }
  def solve(v: Long) {
    assert(ish)
    // calculate residual
    val r = op match {
      case "+" => if (lhs.ish) v - rhs.doop() else v - lhs.doop()
      case "-" => if (lhs.ish) v + rhs.doop() else lhs.doop() - v
      case "*" => if (lhs.ish) v / rhs.doop() else v / lhs.doop()
      case "/" => if (lhs.ish) v * rhs.doop() else lhs.doop() / v
      case _ => { println(f"part2: ${v}"); v }
    }
    // pass down..
    if (lhs!=null && lhs.ish)
      lhs.solve(r)
    else if (rhs!=null && rhs.ish)
      rhs.solve(r)
  }
}

object Main {
  def gentree(name: String, loadmap: HashMap[String,Array[String]]): Monkey = {
    val toks = loadmap(name)
    toks.length match {
      case 2 => new Monkey(name,null,null,toks(1))
      case 4 => new Monkey(name,gentree(toks(1),loadmap),gentree(toks(3),loadmap),toks(2))
      case _ => sys.error(f"${toks}")
    }
  }
  def main(args: Array[String]): Unit = {
    val loadmap: HashMap[String,Array[String]] = HashMap()
    var line = StdIn.readLine()
    while (line != null) {
      val toks = line.split(" ")
      val name = toks(0).substring(0,toks(0).length-1)
      loadmap(name) = toks
      line = StdIn.readLine()
    }
    // process loaded map into a tree of Monkeys
    val root = gentree("root", loadmap)
    // calculate final value
    val part1 = root.doop()
    println(f"part1: ${part1}")
    // find path to humn
    root.find()
    // solve for equality at root..
    val lhs = root.lhs
    val rhs = root.rhs
    if (lhs.ish) lhs.solve(rhs.doop()) else rhs.solve(lhs.doop())
  }
}
