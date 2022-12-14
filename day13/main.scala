// Recursive packet comparator..
import scala.io.StdIn

class Packet(val text:String, val verb:Boolean=false) {
  // how much text was consumed to create this?
  private var len = 0
  // do we have a simple value or a sub packet?
  var value = -1
  var items:List[Packet] = List()
  if (verb) print("PKT=")
  if (text.charAt(0)=='[') {
    if (verb) print('[')
    var off = 1
    while (off<text.length && text.charAt(off)!=']') {
      val pkt = new Packet(text.substring(off), verb)
      items = items :+ pkt
      off += pkt.len
      if (text.charAt(off)==',') {
        off += 1
        if (verb) print(',')
      }
    }
    off += 1
    len = off
    if (verb) print(']')
  } else {
    var off = 1
    while (off<text.length && text.charAt(off)!=',' && text.charAt(off)!=']')
      off += 1
    val sub = text.substring(0,off)
    if (sub.length>0)
      value = sub.toInt
    len = off
    if (verb) print(value)
  }

  def this(v:Int, verb:Boolean) = {
    this("["+v+"]", verb)
  }

  // returns: -1 = out of order, 0 = undecided, 1 = in order
  def inOrder(rhs:Packet, pfx:String=""): Int = {
    // the rules:
    // - both simple values: l>r=>out, l==r=>und, l<r=>in
    // - both lists: recurse while und or: l.length>r.length=>out, l.length==r.length=>und, l.length<r.length=>in
    // - one simple: value=>list, and proceed as above
    var res = 0
    if (value> -1 && rhs.value> -1) { // both simple
      res = if (value>rhs.value) -1 else if (value<rhs.value) 1 else 0
      if (verb) println(f"${pfx}S(${value}/${rhs.value})=${res}")
    } else if (value> -1 && rhs.value<0) {
      print(f"${pfx}CL(${value})=")
      val lh = new Packet(value,verb)
      if (verb) println()
      res = lh.inOrder(rhs,pfx)
    } else if (value<0 && rhs.value> -1) {
      print(f"${pfx}CR(${rhs.value})=")
      val rh = new Packet(rhs.value,verb)
      if (verb) println()
      res = inOrder(rh,pfx)
    } else {
      val mix = items.zip(rhs.items)
      if (verb) println(f"${pfx}L(${mix.length})")
      for (m <- mix) {
        val (l,r) = m
        if (res==0)
          res = l.inOrder(r,pfx+"  ")
      }
      if (res==0) {
        res = if (items.length>rhs.items.length) -1 else if (items.length<rhs.items.length) 1 else 0
        println(f"${pfx}LL(${items.length}/${rhs.items.length})=${res}")
      }
    }
    return res
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val verb = if (args.length>0) true else false
    // collect parsed packet lists
    var list1:List[Packet] = List()
    var list2:List[Packet] = List()
    // parse line pairs from input
    var text1 = StdIn.readLine()
    var text2 = StdIn.readLine()
    var text_ = StdIn.readLine()
    while (text1 != null && text2 != null) {
      if (verb) print(text1+" => ")
      list1 = list1 :+ new Packet(text1, verb)
      if (verb) println()
      if (verb) print(text2+" => ")
      list2 = list2 :+ new Packet(text2, verb)
      if (verb) println()
      if (verb) println()
      text1 = StdIn.readLine()
      text2 = StdIn.readLine()
      text_ = StdIn.readLine()
    }
    // part1: add sequence numbers of in order packets
    var sum = 0
    var idx = 1
    for (p <- list1.zip(list2)) {
      val (l,r) = p
      if (verb) println("Pair:"+idx)
      val i = l.inOrder(r)
      if (i>0)
        sum += idx
      idx += 1
      if (verb) println()
    }
    println("part1:"+sum)
  }
}
