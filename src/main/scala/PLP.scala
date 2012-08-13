object PLP extends App {
import collection.immutable.TreeSet
import scala.collection.mutable.SynchronizedPriorityQueue
import scala.actors.Actor._
import scala.util.control.Exception.{ allCatch, catching }
import System.{ currentTimeMillis => now }

sealed trait Locatable {
  val l, w, filled, fixed: Int
  val area: Int = l * w
  def rotate = Plate(w, l, Plate(0, 0, Nil) :: this :: Nil)
  def shrink = this match {
    case b: Box => b
    case p: Plate => Locatable.shrink(p.blocks)
  }
  def concat(b: Locatable) =
    if (b.area == 0) this
    else Plate(l + b.w, w, this :: b :: Nil)
}

object Locatable {
  def shrink: PartialFunction[Seq[Locatable], Locatable] = {
    case Nil => Plate(0, 0, Nil)
    case b1 :: Nil => b1
    case bs @ b1 :: b2 :: Nil =>
      Plate((b1.l + b2.w).max(b2.w), b2.l.max(b1.w).max(b1.w), bs)
    case bs @ b1 :: b2 :: b3 :: Nil =>
      Plate((b1.l + b2.w).max(b3.l).max(b2.w),
            (b2.l + b3.w).max(b1.w).max(b1.w + b3.w), bs)
    case bs @ b1 :: b2 :: b3 :: b4 :: Nil =>
      Plate((b1.l + b2.w).max(b3.l + b4.w).max(b2.w + b4.w),
            (b2.l + b3.w).max(b4.l + b1.w).max(b1.w + b3.w), bs)
    case bs @ b1 :: b2 :: b3 :: b4 :: b5 :: Nil =>
      Plate((b1.l + b2.w).max(b3.l + b4.w).max(b2.w + b4.w + b5.l),
            (b2.l + b3.w).max(b4.l + b1.w).max(b1.w + b3.w + b5.w), bs)
  }
  def flatten(p: Locatable): Seq[Box] = p match {
    case b: Box => b :: Nil
    case b: Plate => b.blocks.flatMap(flatten)
  }
}

case class Box(name: Symbol, l: Int, w: Int) extends Locatable {
  assert(l >= w)
  val filled = area
  val fixed = area
}

implicit object Box extends Ordering[Box] {
  def apply(name: Char, l: Int, w: Int) = new Box(Symbol(name.toString), l.max(w), l.min(w))
  def compare(x: Box, y: Box) = if (x.filled < y.filled) -1 else if (x.filled > y.filled) 1 else 0
}

case class Plate(l: Int, w: Int, blocks: Seq[Locatable]) extends Locatable {
  lazy val filled: Int = blocks./:(0)(_ + _.filled)
  lazy val ratio: Double = if (area > 0) filled / area.toDouble else 0
  lazy val fixed: Int = blocks match {
    case Nil => 0
    case b1 :: Nil => b1.fixed
    case b1 :: b2 :: Nil => b1.fixed + b2.fixed
    case b1 :: b2 :: b3 :: Nil => b1.area + b2.fixed + b3.fixed
    case b1 :: b2 :: b3 :: b4 :: Nil => b1.area + b2.area + b3.fixed + b4.fixed
    case b1 :: b2 :: b3 :: b4 :: b5 :: Nil => b1.area + b2.area + b3.area + b4.fixed + b5.fixed
  }
  def show() = println("area=%d, blank=%d, filled=%d, fill-ratio=%f, boxes=%s".format(
    area, area - filled, filled, ratio, Locatable.flatten(this).map(_.name).mkString(",")))
}

implicit def pimpTreeSet(boxes: TreeSet[Box]) = new AnyRef {
  def -(elem: Locatable): TreeSet[Box] = elem match {
    case b: Box => boxes - b
    case p: Plate => boxes -- flatten(p)
  }
  def --(elems: TraversableOnce[Locatable]): TreeSet[Box] = elems./:(boxes){
    case (s, b: Box) => s - b
    case (s, p: Plate) => s -- flatten(p)
  }
  def flatten(p: Plate): Seq[Box] = p.blocks.flatMap{
    case b: Box => b :: Nil
    case p: Plate => flatten(p)
  }
}

sealed trait Context {
  val l, w: Int
  val area = l * w
  def filled: Int
  val boxes: TreeSet[Box]
  val revert, short: Plate => Context
  val predict: Int
}

case class FillContext(l: Int, w: Int, boxes: TreeSet[Box], blocks: Seq[Locatable],
                       revert: Locatable => Context, short: Locatable => DoneContext) extends Context {
  def explode: Seq[Context] =
    boxes.filter(b => b.l <= spiralArea._1 && b.w <= spiralArea._2).toSeq match {
      case Nil => revert(Locatable.shrink(blocks)) :: Nil
      case bs => bs.map(chain)
    }
  def spiralArea = blocks match {
    case Nil => (l, w)
    case b1 :: Nil => (w, l - b1.l)
    case b1 :: b2 :: Nil => (l, w - b2.l.max(b1.w))
    case b1 :: b2 :: b3 :: Nil => (w - b1.w, l - b3.l.max(b2.w))
    case b1 :: b2 :: b3 :: b4 :: Nil => (l - b2.w - b4.w, w - b1.w - b3.w)
  }
  def chain: Box => FillContext = blocks match {
    case Nil => (b1: Box) =>
      FillContext(l, w, boxes - b1, Seq(b1), b => revert(b), b => short(b))
    case bs @ b1 :: Nil => (b2: Box) => {
      val (nl, nw, loc) = (b1.w, l - b1.l - b2.w) match {
        case (nl, nw) if nl >= nw => (nl, nw, stretchLocater(b2, bs))
        case (nl, nw) => (nw, nl, stretchRotateLocater(b2, bs))
      }
      FillContext(nl, nw, boxes -- (b2 :: bs), Nil, chainRevert(loc), chainShort(loc))
    }
    case bs @ b1 :: b2 :: Nil => (b3: Box) => {
      val (nl, nw, loc) = (b2.w, w - b2.l - b3.w) match {
        case (nl, nw) if nl >= nw => (nl, nw, stretchLocater(b3, bs))
        case (nl, nw) => (nw, nl, stretchRotateLocater(b3, bs))
      }
      FillContext(nl, nw, boxes -- (b3 :: bs), Nil, chainRevert(loc), chainShort(loc))
    }
    case bs @ b1 :: b2 :: b3 :: Nil => (b4: Box) => {
      val (nl, nw, loc) = (b3.w, l - b3.l - b4.w) match {
        case (nl, nw) if nl >= nw => (nl, nw, stretchLocater(b4, bs))
        case (nl, nw) => (nw, nl, stretchRotateLocater(b4, bs))
      }
      FillContext(nl, nw, boxes -- (b4 :: bs), Nil, chainRevert(loc), chainShort(loc))
    }
    case bs @ b1 :: b2 :: b3 :: b4 :: Nil => (b5: Box) => {
      val (nl, nw, loc) = (b4.w, w - b4.l - b1.w) match {
        case (nl, nw) if nl >= nw => (nl, nw, stretchLocater(b5, bs))
        case (nl, nw) => (nw, nl, stretchRotateLocater(b5, bs))
      }
      val shrinked = Locatable.shrink compose loc
      FillContext(nl, nw, boxes -- (b5 :: bs), Nil, b => revert(shrinked(b)), chainShort(loc))
    }
  }
  def chainRevert(locate: Locatable => Seq[Locatable]) =
    (b: Locatable) => { val bs = locate(b); FillContext(l, w, boxes -- bs, bs, revert, short) }
  def chainShort(locate: Locatable => Seq[Locatable]) =
    (b: Locatable) => short(Plate(l, w, locate(b)))
  def stretchLocater(p: Locatable, bs: Seq[Locatable]): Locatable => Seq[Locatable] =
    (b: Locatable) => bs.init ++ Seq(bs.last.concat(b), p)
  def stretchRotateLocater(p: Locatable, bs: Seq[Locatable]): Locatable => Seq[Locatable] =
    (b: Locatable) => bs.init ++ Seq(bs.last.concat(b.rotate), p)
  val filled = blocks.map(_.filled).sum
  val terminate = short(Plate(l, w, blocks))
  val interim = terminate.result
  val priority = interim.filled
  // val priority = if (interim.fixed > 0) interim.filled / interim.fixed else 0
  val predict = interim.area - interim.fixed + interim.filled
}

implicit object FillContext extends Ordering[FillContext] {
  def apply(l: Int, w: Int, boxes: TreeSet[Box]) = {
    val short = (result: Locatable) => result match {
      case b: Plate => DoneContext(l, w, boxes - b, b)
      case b: Box => DoneContext(l, w, boxes - b, Plate(l, w, b :: Nil))
    }
    new FillContext(l, w, boxes, Nil, short, short)
  }
  def compare(x: FillContext, y: FillContext) =
    if (x.priority > y.priority) -1 else if (x.priority < y.priority) 1 else 0
}

case class DoneContext(l: Int, w: Int, boxes: TreeSet[Box], result: Plate) extends Context {
  val short = (x: Plate) => this
  val revert = short
  val predict, filled = result.filled
}

implicit object DoneContext extends Ordering[DoneContext] {
  def compare(x: DoneContext, y: DoneContext) =
    if (x.result.ratio > y.result.ratio) -1 else if (x.result.ratio < y.result.ratio) 1 else 0
}

case class Packer(boxes: TreeSet[Box], var loglevel: Int=0) {
  val tasks = new SynchronizedPriorityQueue[FillContext]()(implicitly[Ordering[FillContext]].reverse)
  val done = new SynchronizedPriorityQueue[DoneContext]()(implicitly[Ordering[DoneContext]].reverse)
  def best: Option[Plate] = done.headOption.map(_.result)
  def threshold = best.map(_.filled).sum
  def run(l: Int, w: Int, timeout: Long=60*1000, multiplicity: Int=10): Option[Plate] = {
    tasks.clear
    done.clear
    tasks.enqueue(FillContext(l, w, boxes))
    cont(timeout, multiplicity)
  }
  def cont(timeout: Long=1000, multiplicity: Int=10, _loglevel: Int=loglevel): Option[Plate] = {
    loglevel = _loglevel
    val workers = (0 to multiplicity).map(worker(_, timeout))
    workers.foreach(_ ! 'Wake)
    try { Thread.sleep(timeout) }
    finally { workers.foreach(_ ! 'End) }
    println("\n" + "task: remains=%d (head priority=%d, predict=%d)".format(
      tasks.size, tasks.headOption.map(_.priority).sum, tasks.headOption.map(_.predict).sum))
    println("best: filled=%d, ratio=%f, %s".format(
      best.map(_.filled).sum, best.map(_.ratio).sum, best.map(_.blocks)))
    best
  }
  def worker(n: Int, timeout: Long) = actor{
    loop {
      react {
        case 'Wake =>
          for {
            ctx <- catching(classOf[NoSuchElementException]).opt(tasks.dequeue)
            if ctx.predict > threshold
            shift <- ctx.explode
            if shift.predict > threshold
          } shift match {
            case x: DoneContext =>
              done.enqueue(x)
              if (loglevel == 0) print(";")
              else println("Done(%d): %f, %d, %d, %s".format(
                tasks.length, x.result.ratio, x.l, x.w, x.result))
            case x: FillContext if x.interim.filled > threshold =>
              done.enqueue(x.terminate)
              tasks.enqueue(x)
              if (loglevel == 0) print(".")
              else println("Fill(%d): %f, %d, %d, %s".format(
                tasks.length, x.interim.ratio, x.l, x.w, x.interim))
            case x: FillContext =>
              tasks.enqueue(x)
              if (loglevel > 1) println("Fill(%d): %f, %d, %d, %s".format(
                tasks.length, x.interim.ratio, x.l, x.w, x.interim))
          }
          self ! 'Wake
        case 'End => exit
      }
    }
  }
}

override def main(args: Array[String]): Unit = {
  val packer = Packer(boxes)
  allCatch.opt(args.map(_.toInt)).flatMap{
    case Array(l, w) => packer.run(l, w)
    case Array(l, w, timeout) => packer.run(l, w, timeout)
    case Array(l, w, timeout, multiplicity) => packer.run(l, w, timeout, multiplicity)
    case _ => { System.err.println("Error: Invalid args."); None }
  }.map(_.show)
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

}
