package de.sciss.plp
  
import java.awt.EventQueue

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.SynchronizedPriorityQueue
import scala.util.control.Exception.{allCatch, catching}

object PLP {
  
  sealed trait Locatable {
    val l, w, filled, fixed: Int
    val area: Int = l * w
    val dead: Int = fixed - filled
    val predict: Int = area - dead
    def rotate: Locatable = {
      Plate(w, l, Plate(0, 0, Nil) :: this :: Nil)
    }
    def shrink: Locatable
    def concat(b: Locatable): Locatable = if (b.area == 0) this else Locatable.shrink(this :: b :: Nil)
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
    val filled, fixed = area
    def shrink: Locatable = this
  }
  
  implicit object Box extends Ordering[Box] {
    def apply(name: Char, l: Int, w: Int) = new Box(Symbol(name.toString), l.max(w), l.min(w))
    def compare(x: Box, y: Box): Int = (x.filled - y.filled).signum
  }
  
  case class Plate(l: Int, w: Int, blocks: Seq[Locatable]) extends Locatable {
    lazy val filled: Int = blocks./:(0)(_ + _.filled)
    lazy val ratio: Double = if (area > 0) filled / area.toDouble else 0
    lazy val fixed: Int = 
      if (blocks.isEmpty) 0
      else blocks.init.map(_.area).sum + blocks.lastOption.map(_.fixed).sum
    def shrink: Locatable = Locatable.shrink(blocks)
    def stat: String = "area=%d, predict=%d, filled=%d, blank=%d, fill-ratio=%f, boxes=%s".format(
      area, predict, filled, area - filled, ratio, Locatable.flatten(this).map(_.name).mkString(","))
  }
  
  implicit class pimpSortedSet(private val boxes: SortedSet[Box]) extends AnyVal {
    def -(elem: Locatable): SortedSet[Box] = elem match {
      case b: Box => boxes - b
      case p: Plate => boxes -- flatten(p)
    }
    def --(elems: TraversableOnce[Locatable]): SortedSet[Box] = elems./:(boxes){
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
    val area: Int = l * w
    def filled: Int
    val boxes: SortedSet[Box]
    val revert, short: Plate => Context
    val predict: Int
  }
  
  case class FillContext(l: Int, w: Int, boxes: SortedSet[Box], blocks: Seq[Locatable],
                         revert: Locatable => Context, short: Locatable => DoneContext,
                         allowRotation: Boolean) extends Context {
    def explode: Seq[Context] =
      boxes.filter(b => b.l <= space._1 && b.w <= space._2).toSeq match {
        case Nil => revert(Plate.apply(l, w, blocks)) :: Nil
        case bs => bs.map(chain)
      }
    lazy val space: (Int, Int) = blocks match {
      case Nil                                  => (l, w)
      case p1                            :: Nil => (w, l - p1.l)
      case p1 :: p2                      :: Nil => (l, w - p2.l.max(p1.w))
      case p1 :: p2 :: p3                :: Nil => (w - p1.w, l - p3.l.max(p2.w))
      case p1 :: _  :: _  :: (b4: Box  ) :: Nil => (b4.w, w - b4.l - p1.w)
      case p1 :: p2 :: p3 :: (p4: Plate) :: Nil => (l - p2.w - p4.w, w - p1.w - p3.w)
    }
    lazy val chain: Box => FillContext = blocks match {
      case Nil => (b1: Box) =>
        FillContext(l, w, boxes - b1, Seq(b1), revert, short, allowRotation = allowRotation)
      case bs @ b1 :: Nil => (b2: Box) =>
        shift(b1.w, l - b1.l - b2.w, boxes -- bs - b2, Nil, p1 => Seq(b1 concat p1, b2))
      case bs @ p1 :: b2 :: Nil => (b3: Box) =>
        shift(b2.w, w - b2.l - b3.w, boxes -- bs - b3, Nil, p2 => Seq(p1, b2 concat p2, b3))
      case bs @ p1 :: p2 :: b3 :: Nil => (b4: Box) =>
        shift(b3.w, l - b3.l - b4.w, boxes -- bs - b4, Nil, p3 => Seq(p1, p2, b3 concat p3, b4))
      case bs @ p1 :: p2 :: p3 :: (b4: Box) :: Nil => (p4: Box) =>
        shift(space._1, space._2, boxes -- bs - p4, Seq(p4), p4 => Seq(p1, p2, p3, b4 concat p4))
      case bs @ p1 :: p2 :: p3 :: (p4: Plate) :: Nil => (b5: Box) =>
        shiftRevert(space._1, space._2, boxes -- bs - b5, Seq(b5), p5 => Seq(p1, p2, p3, p4, p5))
    }
    val cons: (Seq[Locatable]) => FillContext = (bs: Seq[Locatable]) =>
      FillContext(l, w, boxes -- bs, bs, revert, short, allowRotation = allowRotation)

    def shift(bl: Int, bw: Int, bs: SortedSet[Box], ps: Seq[Locatable],
              locator: Locatable => Seq[Locatable], cons: Seq[Locatable] => Context = cons): FillContext = {
      val (pl, pw, relocator) = if (bl >= bw || !allowRotation)
        (bl, bw, locator)
      else
        (bw, bl, locator compose ((b: Locatable) => b.rotate))

      FillContext(pl, pw, bs, Nil, cons compose relocator,
        short compose Locatable.shrink compose relocator, allowRotation = allowRotation)
    }

    def shiftRevert(bl: Int, bw: Int, bs: SortedSet[Box], ps: Seq[Locatable], locator: Locatable => Seq[Locatable]): FillContext =
      shift(bl, bw, bs, ps, locator, revert compose Locatable.shrink)
    val done = short(Locatable.shrink(blocks))
    val interim : Plate = done.result
    val priority: Int   = interim.filled
    val filled  : Int   = interim.filled
    val predict : Int   = interim.predict
  }
  
  implicit object FillContext extends Ordering[FillContext] {
    def apply(l: Int, w: Int, boxes: SortedSet[Box], allowRotation: Boolean): FillContext = {
      val short = (b: Locatable) => DoneContext(l, w, boxes - b, Plate(l, w, b :: Nil))
      new FillContext(l, w, boxes, Nil, short, short, allowRotation = allowRotation)
    }
    def compare(x: FillContext, y: FillContext): Int = (y.priority - x.priority).signum
  }
  
  case class DoneContext(l: Int, w: Int, boxes: SortedSet[Box], result: Plate) extends Context {
    val short : (Plate) => DoneContext = _ => this
    val revert: (Plate) => DoneContext = short
    val predict, filled = result.filled
  }
  
  implicit object DoneContext extends Ordering[DoneContext] {
    def compare(x: DoneContext, y: DoneContext): Int = (y.filled - x.filled).signum
  }
  
  case class Packer(boxes: SortedSet[Box], var logLevel: Int = 0, allowRotation: Boolean = true) {
    val tasks: mutable.PriorityQueue[FillContext] = new SynchronizedPriorityQueue[FillContext]()(implicitly[Ordering[FillContext]].reverse)
    val done : mutable.PriorityQueue[DoneContext] = new SynchronizedPriorityQueue[DoneContext]()(implicitly[Ordering[DoneContext]].reverse)
    def best: Option[Plate] = done.headOption.map(_.result)
    def threshold: Int = best.map(_.filled).sum

    def run(l: Int, w: Int, timeout: Long = 60L * 1000, multiplicity: Int = 10): Option[Plate] = {
      tasks.clear()
      done .clear()
      tasks.enqueue(FillContext(l, w, boxes, allowRotation = allowRotation))
      cont(timeout, multiplicity)
    }

    def cont(timeout: Long = 1000, multiplicity: Int = 10, _logLevel: Int = logLevel): Option[Plate] = {
      logLevel = _logLevel
      val workers = (0 to multiplicity).map(worker(_, timeout))
      workers.foreach(_ ! 'Wake)
      try { Thread.sleep(timeout) }
      finally { workers.foreach(_ ! 'End) }
      println("\n" + "task: remains=%d (head priority=%d, predict=%d)".format(
        tasks.size, tasks.headOption.map(_.priority).sum, tasks.headOption.map(_.predict).sum))
      println("best: " + best.map(_.stat).getOrElse("None"))
      best
    }

    private[this] val actorSystem = ActorSystem("workers")

    def worker(n: Int, timeout: Long): ActorRef =
      actorSystem.actorOf(Props[Worker](new Worker(n, timeout, this)), s"worker-$n")
  }

  class Worker(n: Int, timeout: Long, packer: Packer) extends Actor {
    import packer.{done, logLevel, tasks, threshold}

    def receive: Receive = {
      case 'Wake =>
        for {
          ctx <- catching(classOf[NoSuchElementException]).opt(tasks.dequeue)
          if ctx.predict >= threshold
          shift <- ctx.explode
          if shift.predict >= threshold
        } shift match {
          case x: DoneContext =>
            done.enqueue(x)
            if (logLevel == 0) print("!")
            else println("Done(%d): %d, %s".format(tasks.length, x.filled, x.result))
          case x: FillContext if x.filled > threshold =>
            done.enqueue(x.done)
            tasks.enqueue(x)
            if (logLevel == 0) print(".")
            else println("Interim(%d): %d, %s".format(tasks.length, x.filled, x.interim))
          case x: FillContext =>
            tasks.enqueue(x)
            if (logLevel > 1) println("Fill(%d): %d, %s".format(tasks.length, x.filled, x.interim))
        }
        self ! 'Wake
      case 'End => context.stop(self)
    }
  }

  def main(args: Array[String]): Unit = {
    val packer = Packer(boxes /* XXX TODO -- doesn't work , allowRotation = false */)
    val res = allCatch.opt(args.map(_.toInt)).flatMap {
      case Array(l, w)                        => packer.run(l, w)
      case Array(l, w, timeout)               => packer.run(l, w, timeout)
      case Array(l, w, timeout, multiplicity) => packer.run(l, w, timeout, multiplicity)
      case _ =>
        System.err.println(
          """Error: Invalid args. Must be one of:
            |
            |<length> <width>
            |<length> <width> <timeout>
            |<length> <width> <timeout> <multiplicity>
            |
            |Where all arguments are integers.
            |- timeout is in milliseconds (default: 1 minute)
            |
            |""".stripMargin)
        sys.exit(1)
    }
    res.foreach { pl =>
      println("Done.")
//      val flat = Locatable.flatten(pl)
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = new PLPFrame(pl /* XXX TODO -- doesn't work , allowRotation = false */)
      })
    }
  }
  
  lazy val boxes: SortedSet[Box] = SortedSet(('a' to 'z') ++ ('A' to 'Z') zip List(
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
