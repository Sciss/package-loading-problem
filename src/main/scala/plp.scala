object PLP extends App {
import collection.immutable.TreeSet
import scala.util.control.Breaks.{ break, breakable }

trait Locatable {
  val l: Int
  val w: Int
  val filled: Int
  val area: Int = l * w
  def score: Double = if (area > 0) filled / area.toDouble else 0
}

implicit object Locatable  extends Ordering[Locatable] {
  def compare(x: Locatable, y: Locatable) =
    if (x.filled < y.filled) -1 else if (x.filled > y.filled) 1 else 0
}

case class Box(name: Symbol, l: Int, w: Int) extends Locatable {
  assert(l >= w)
  val filled = area
}

implicit object Box extends Ordering[Box] {
  val Empty = Box('Empty, 0, 0)
  def apply(name: Char, l: Int, w: Int) = new Box(Symbol(name.toString), l.max(w), l.min(w))
  def compare(x: Box, y: Box) = Locatable.compare(x, y)
}

type FiveBlocks = IndexedSeq[Option[Locatable]]

case class Plate(l: Int, w: Int, blocks: FiveBlocks) extends Locatable {
  lazy val filled = blocks.flatten./:(0)(_ + _.filled)
}

implicit def pimpTreeSet(boxes: TreeSet[Box]) = new AnyRef {
  def contains(elem: Locatable): Boolean = elem match {
    case b: Box => boxes.contains(b)
    case p: Plate => p.blocks.forall{
      case None => true
      case Some(x) => contains(x)
    }
  }
  def - (elem: Locatable): TreeSet[Box] = elem match {
    case b: Box => boxes - b
    case p: Plate => boxes -- flatten(p)
  }
  def flatten(p: Plate): IndexedSeq[Box] = p.blocks.collect{
    case Some(b) => b
  }.flatMap{
    case b: Box => b :: Nil
    case p: Plate => flatten(p)
  }
}

def pack(l: Int, w: Int, boxes: TreeSet[Box]): Iterable[Locatable] = {
  def pull(xs1: Iterable[Locatable], xs2: Iterable[Locatable]): Stream[Locatable] = {
    (xs1.headOption, xs2.headOption) match {
      case (Some(s), Some(b)) if s.score > b.score => Stream.cons(s, pull(xs1.tail, xs2))
      case (_, Some(b)) => Stream.cons(b, pull(xs1, xs2.tail))
      case (Some(s), None) => Stream.cons(s, pull(xs1.tail, xs2))
      case (None, None) => Stream.empty
    }
  }
  if (l == 0 || w == 0) Stream.empty
  else pull(bigPadding(l, w, boxes), split(l, w, boxes))
}

def big(l: Int, w: Int, boxes: TreeSet[Box]): Iterable[Box] = 
  boxes.view.withFilter(b => b.area <= l * w && b.l > l / 2 && b.l <= l && b.w <= w)

def bigPadding(l: Int, w: Int, boxes: TreeSet[Box]): Iterable[Locatable] = {
  import Box.Empty
  for {
    b1 <- big(l, w, boxes); r2 = boxes - b1
    b2 <- pack(w, l - b1.l, r2) ++ Seq(Empty); r3 = r2 - b2
    b3 <- pack(l, w - b2.l.max(b1.w), r3) ++ Seq(Empty); r4 = r3 - b3
    b4 <- pack(w - b1.w, l - b3.l.max(b2.w), r4) ++ Seq(Empty)
    b5 <- pack(0.max(b1.l + b3.l - l), 0.max(b2.w + b4.w - w), r4 - b4) ++ Seq(Empty)
  } yield Vector(b1, b2, b3, b4, b5) match {
    case Vector(x, Empty, Empty, Empty, Empty) => x
    case xs => Plate(l, w, xs.map{ case Empty => None; case x => Some(x) })
  }
}

def split(l: Int, w: Int, boxes: TreeSet[Box]): Iterable[Plate] = {
  import Box.Empty
  for {
    b1 <- pack(l / 2, w, boxes); r2 = boxes - b1
    b2 <- pack(w, l - b1.l, r2); r3 = r2 - b2
    b3 <- pack(l, w - b2.l.max(b1.w), r3) ++ Seq(Empty); r4 = r3 - b3
    b4 <- pack(w - b1.w, l - b3.l.max(b2.w), r4) ++ Seq(Empty)
    b5 <- pack(0.max(b1.l + b3.l - l), 0.max(b2.w + b4.w - w), r4 - b4) ++ Seq(Empty)
  } yield Plate(l, w, Vector(b1, b2, b3, b4, b5).map{ case Empty => None; case x => Some(x) })
}

def run(l: Int, w: Int, timeout: Int=60*1000): Locatable = {
  import System.{ currentTimeMillis => now }
  val s = l * w
  val start = now
  def log(x: Locatable, current: Long) =
    println("area = %d, blank = %d, fill-ratio = %f, time = %d[ms]\n".format(
      s, s - x.filled, x.filled / s.toDouble, current - start) + x)
  var result: Locatable = Box.Empty
  var num = 0
  breakable {
    for (x <- pack(l, w, boxes)) {
      num += 1
      if (result.filled <= x.filled) log(x, now)
      result = result max x
      if (now - start > timeout) break
    }
  }
  println("---------- %d combinations tested ----------".format(num))
  log(result, now)
  result
}

lazy val boxes = TreeSet(('a' to 'z') ++ ('A' to 'Z') zip List(
  (42, 18), (35,  1), (20, 25), (29,  9), (13, 15), ( 6, 46), (32, 28),
  (12, 42), (46, 43), (28, 37), (42,  5), ( 3,  4), (43, 33), (22, 17),
  (19, 46), (48, 27), (22, 39), (20, 13), (18, 50), (36, 45), ( 4, 12),
  (23, 34), (24, 15), (42, 12), ( 4, 19), (48, 45), (13,  8), (38, 10),
  (24, 42), (30, 29), (17, 36), (41, 43), (39,  7), (41, 43), (15, 49),
  (47,  6), (41, 30), (21,  1), ( 7,  2), (44, 49), (30, 24), (35,  5),
  ( 7, 41), (17, 27), (32,  9), (45, 40), (27, 24), (38, 39), (19, 33),
  (30, 42), (34, 16), (40,  9)
) map { case (id, (l,  w)) => Box(id, l, w) }: _*)(implicitly[Ordering[Box]].reverse)

import scala.util.control.Exception.allCatch
override def main(args: Array[String]): Unit = allCatch.opt(args.map(_.toInt)).map {
  case Array(w, l) => Some(run(w, l))
  case Array(w, l, timeout) => Some(run(w, l, timeout))
  case _ => { System.err.println("Error: Invalid arguments."); None }
}

}
