object PLP extends App {
import collection.immutable.TreeSet
import scala.collection.mutable.SynchronizedPriorityQueue
import scala.actors.Actor._
import scala.util.control.Exception.{ allCatch, catching }
import System.{ currentTimeMillis => now }

sealed trait Locatable {
  val l, w, filled, fixed: Int
  val area: Int = l * w
  val dead = fixed - filled
  val predict = area - dead
  def rotate = Plate(w, l, Plate(0, 0, Nil) :: this :: Nil)
  def shrink: Locatable
  def concat(b: Locatable) = if (b.area == 0) this else Locatable.shrink(this :: b :: Nil)
}

object Locatable {
  def shrink: PartialFunction[Seq[Locatable], Locatable] = {
    case Nil => Plate(0, 0, Nil)
    case b1 :: Nil => b1
    case bs @ b1 :: b2 :: Nil =>
      Plate(b1.l + b2.w, b2.l.max(b1.w), bs)
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
  def shrink: Locatable = this
}

implicit object Box extends Ordering[Box] {
  def apply(name: Char, l: Int, w: Int) = new Box(Symbol(name.toString), l.max(w), l.min(w))
  def compare(x: Box, y: Box) = if (x.filled < y.filled) -1 else if (x.filled > y.filled) 1 else 0
}

case class Plate(l: Int, w: Int, blocks: Seq[Locatable]) extends Locatable {
  lazy val filled: Int = blocks./:(0)(_ + _.filled)
  lazy val ratio: Double = if (area > 0) filled / area.toDouble else 0
  lazy val fixed: Int = 
    if (blocks.isEmpty) 0
    else blocks.init.map(_.area).sum + blocks.lastOption.map(_.fixed).sum
  def shrink: Locatable = Locatable.shrink(blocks)
  def show() = println("area=%d, predict=%d, filled=%d, blank=%d, fill-ratio=%f, boxes=%s".format(
    area, predict, filled, area - filled, ratio, Locatable.flatten(this).map(_.name).mkString(",")))
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
      case Nil => revert(Plate.apply(l, w, blocks)) :: Nil
      case bs => bs.map(chain)
    }
  lazy val spiralArea = blocks match {
    case Nil => (l, w)
    case p1 :: Nil => (w, l - p1.l)
    case p1 :: p2 :: Nil => (l, w - p2.l.max(p1.w))
    case p1 :: p2 :: p3 :: Nil => (w - p1.w, l - p3.l.max(p2.w))
    case p1 :: p2 :: p3 :: (b4: Box) :: Nil => (b4.w, w - b4.l - p1.w)
    case p1 :: p2 :: p3 :: (p4: Plate) :: Nil => (l - p2.w - p4.w, w - p1.w - p3.w)
  }
  def chain: Box => FillContext = blocks match {
    case Nil => (b1: Box) => {
      FillContext(l, w, boxes - b1, Seq(b1), revert, short)
    }
    case bs @ b1 :: Nil => (b2: Box) => {
      val (p1l, p1w, p1locator) = (b1.w, l - b1.l - b2.w) match {
        case (p1l, p1w) if p1l >= p1w => (p1l, p1w, (p1: Locatable) => Seq(b1 concat p1, b2))
        case (p1l, p1w) => (p1w, p1l, (p1: Locatable) => Seq(b1 concat p1.rotate, b2))
      }
      FillContext(p1l, p1w, boxes -- bs - b2, Nil,
                  ((p1b2: Seq[Locatable]) => FillContext(l, w, boxes -- p1b2, p1b2, revert, short)) compose p1locator,
                  short compose Locatable.shrink compose p1locator)
    }
    case bs @ p1 :: b2 :: Nil => (b3: Box) => {
      val (p2l, p2w, p2locator) = (b2.w, w - b2.l - b3.w) match {
        case (p2l, p2w) if p2l >= p2w => (p2l, p2w, (p2: Locatable) => Seq(p1, b2 concat p2, b3))
        case (p2l, p2w) => (p2w, p2l, (p2: Locatable) => Seq(p1, b2 concat p2.rotate, b3))
      }
      FillContext(p2l, p2w, boxes -- bs - b3, Nil,
                  ((p1p2b3: Seq[Locatable]) => FillContext(l, w, boxes -- p1p2b3, p1p2b3, revert, short)) compose p2locator,
                  short compose Locatable.shrink compose p2locator)
    }
    case bs @ p1 :: p2 :: b3 :: Nil => (b4: Box) => {
      val (p3l, p3w, p3locator) = (b3.w, l - b3.l - b4.w) match {
        case (p3l, p3w) if p3l >= p3w => (p3l, p3w, (p3: Locatable) => Seq(p1, p2, b3 concat p3, b4))
        case (p3l, p3w) => (p3w, p3l, (p3: Locatable) => Seq(p1, p2, b3 concat p3.rotate, b4))
      }
      FillContext(p3l, p3w, boxes -- bs - b4, Nil,
                  ((p1p2p3b4: Seq[Locatable]) => FillContext(l, w, boxes -- p1p2p3b4, p1p2p3b4, revert, short)) compose p3locator,
                  short compose Locatable.shrink compose p3locator)
    }
    case bs @ p1 :: p2 :: p3 :: (b4: Box) :: Nil => (p4: Box) => {
      val (p4l, p4w, p4locator) = (spiralArea._1, spiralArea._2) match {
        case (p4l, p4w) if p4l >= p4w => (p4l, p4w, (p4: Locatable) => Seq(p1, p2, p3, b4 concat p4))
        case (p4l, p4w) => (p4w, p4l, (p4: Locatable) => Seq(p1, p2, p3, b4 concat p4.rotate))
      }
      FillContext(p4l, p4w, boxes -- bs - p4, Seq(p4),
                  ((p1p2p3p4: Seq[Locatable]) => FillContext(l, w, boxes -- p1p2p3p4, p1p2p3p4, revert, short)) compose p4locator,
                  short compose Locatable.shrink compose p4locator)
    }
    case bs @ p1 :: p2 :: p3 :: (p4: Plate) :: Nil => (b5: Box) => {
      val (p5l, p5w, p5locator) = (spiralArea._1, spiralArea._2) match {
        case (p5l, p5w) if p5l >= p5w => (p5l, p5w, (p5: Locatable) => Seq(p1, p2, p3, p4, p5))
        case (p5l, p5w) => (p5w, p5l, (p5: Locatable) => Seq(p1, p2, p3, p4, p5.rotate))
      }
      FillContext(p5l, p5w, boxes -- bs - b5, Seq(b5),
                  revert compose Locatable.shrink compose p5locator,
                  short compose Locatable.shrink compose p5locator)
    }
  }
  val filled = blocks.map(_.filled).sum
  val done = short(Locatable.shrink(blocks))
  val interim = done.result
  val priority = interim.filled
  val predict = interim.predict
}

implicit object FillContext extends Ordering[FillContext] {
  def apply(l: Int, w: Int, boxes: TreeSet[Box]) = {
    val short = (b: Locatable) => DoneContext(l, w, boxes - b, Plate(l, w, b :: Nil))
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
    if (x.result.filled > y.result.filled) -1 else if (x.result.filled < y.result.filled) 1 else 0
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
            if ctx.predict >= threshold
            shift <- ctx.explode
            if shift.predict >= threshold
          } shift match {
            case x: DoneContext =>
              done.enqueue(x)
              if (loglevel == 0) print("!")
              else println("Done(%d): %d, %s".format(tasks.length, x.result.filled, x.result))
            case x: FillContext if x.interim.filled > threshold =>
              done.enqueue(x.done)
              tasks.enqueue(x)
              if (loglevel == 0) print(".")
              else println("Interim(%d): %d, %s".format(tasks.length, x.interim.filled, x.interim))
            case x: FillContext =>
              tasks.enqueue(x)
              if (loglevel > 1) println("Fill(%d): %d, %s".format(tasks.length, x.interim.filled, x.interim))
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
