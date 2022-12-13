// Monkey in the Middle - logic simulation..

class Monkey(var items:List[Int], val op:Function1[Int,Int], val test:Function1[Int,Int]) {
  var count: Int = 0
  override def toString = "Monkey(count="+count+", items="+items.toString+")"
}
object Monkey {
  def test =
    List(
      new Monkey(List(79, 98), x => x*19, x => if ((x%23)==0) 2 else 3),
      new Monkey(List(54, 65, 75, 74), x => x+6, x => if ((x%19)==0) 2 else 0),
      new Monkey(List(79, 60, 97), x => x*x, x => if ((x%13)==0) 1 else 3),
      new Monkey(List(74), x => x+3, x => if ((x%17)==0) 0 else 1)
    )
  def input =
    List(
      new Monkey(List(91, 54, 70, 61, 64, 64, 60, 85), x => x*13, x => if ((x%2)==0) 5 else 2),
      new Monkey(List(82), x => x+7, x => if ((x%13)==0) 4 else 3),
      new Monkey(List(84, 93, 70), x => x+2, x => if ((x%5)==0) 5 else 1),
      new Monkey(List(78, 56, 85, 93), x => x*2, x => if ((x%3)==0) 6 else 7),
      new Monkey(List(64, 57, 81, 95, 52, 71, 58), x => x*x, x => if ((x%11)==0) 7 else 3),
      new Monkey(List(58, 71, 96, 58, 68, 90), x => x+6, x => if ((x%17)==0) 4 else 1),
      new Monkey(List(56, 99, 89, 97, 81), x => x+1, x => if ((x%7)==0) 0 else 2),
      new Monkey(List(68, 72), x => x+8, x => if ((x%19)==0) 6 else 0)
    )
}

object Main {
  def main(args: Array[String]): Unit = {
    val monkeys = if (args.length>0 && args(0).startsWith("input")) Monkey.input else Monkey.test
    println(monkeys)
    // Play 20 rounds of keep away..
    for (r <- 1 to 20) {
      println("ROUND "+r)
      // for each monkey in turn..
      var i = 0
      for (m <- monkeys) {
        println("  Monkey "+i)
        i += 1
        // inspect each item, apply logic
        for (i <- m.items) {
          m.count += 1
          val w = m.op(i)/3
          val t = m.test(w)
          monkeys(t).items = monkeys(t).items :+ w
          println("    "+i+" => "+w+" => "+t)
        }
        m.items = List()
      }
      println(monkeys)
    }
    // find two highest counts
    var c1 = 0
    var c2 = 0
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
