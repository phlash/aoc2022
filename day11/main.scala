// Monkey in the Middle - logic simulation..

class Monkey(var items:List[Long], val op:Function1[Long,Long], val div:Long, val yeah:Int, val nah:Int) {
  var count: Long = 0
  override def toString = "Monkey(count="+count+", items="+items.toString+")"
}
object Monkey {
  def test =
    List(
      new Monkey(List(79, 98), x => x*19, 23, 2, 3),
      new Monkey(List(54, 65, 75, 74), x => x+6, 19, 2, 0),
      new Monkey(List(79, 60, 97), x => x*x, 13, 1, 3),
      new Monkey(List(74), x => x+3, 17, 0, 1)
    )
  def input =
    List(
      new Monkey(List(91, 54, 70, 61, 64, 64, 60, 85), x => x*13, 2, 5, 2),
      new Monkey(List(82), x => x+7, 13, 4, 3),
      new Monkey(List(84, 93, 70), x => x+2, 5, 5, 1),
      new Monkey(List(78, 56, 85, 93), x => x*2, 3, 6, 7),
      new Monkey(List(64, 57, 81, 95, 52, 71, 58), x => x*x, 11, 7, 3),
      new Monkey(List(58, 71, 96, 58, 68, 90), x => x+6, 17, 4, 1),
      new Monkey(List(56, 99, 89, 97, 81), x => x+1, 7, 0, 2),
      new Monkey(List(68, 72), x => x+8, 19, 6, 0)
    )
}

object Main {
  def main(args: Array[String]): Unit = {
    val monkeys = if (args.length>0 && args(0).startsWith("input")) Monkey.input else Monkey.test
    val div = if (args.length>1 && args(1).startsWith("part2")) 1 else 3
    println(monkeys)
    var lcm:Long = 1
    for (m <- monkeys)
      lcm = lcm*m.div
    // Play required rounds of keep away..
    for (r <- 1 to (if (div>1) 20 else 10000)) {
      // for each monkey in turn..
      for (m <- monkeys) {
        // inspect each item, apply logic
        for (i <- m.items) {
          m.count += 1
          val w = (m.op(i)/div) % lcm
          val t = if ((w%m.div)==0) m.yeah else m.nah
          monkeys(t).items = monkeys(t).items :+ w
        }
        m.items = List()
      }
      if (1==r || 20==r || (r%1000)==0) {
        print("ROUND "+r+" ")
        println(monkeys)
      }
    }
    // find two highest counts
    var c1:Long = 0
    var c2:Long = 0
    for (m <- monkeys) {
      if (m.count > c1) {
        c2 = c1
        c1 = m.count
      } else if (m.count > c2) {
        c2 = m.count
      }
    }
    println("Counts: c1="+c1+", c2="+c2+", res="+(c1*c2))
  }
}
