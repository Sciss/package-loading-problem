package de.sciss.plp

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.plp.PLP.{Box, Locatable, Plate}

import scala.collection.immutable.SortedSet

object PackImages {
  final case class Config(
                           dpi        : Int     = 72,
                           convert    : String  = "convert",
                           imageDir   : File    = file("images"),
                           output     : File    = file("output.png"),
                           inputIsPDF : Boolean = false,
                           width      : Int     = 400,
                           height     : Int     = 400,
                           timeOut    : Int     = 60,
                           pad        : Int     = 4,
                           tab        : Int     = 4,
                           div        : Int     = 1
                         )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("PackImages") {
      opt[File]('i', "input")
        .text (s"Base input directory of png or pdf images")
        .required()
        .action { (v, c) => c.copy(imageDir = v) }

      opt[File]('o', "output")
        .text ("Output png image path")
        .required()
        .action { (v, c) => c.copy(output = v) }

      opt[Unit] ("pdf")
        .text (s"Input is pdf files (default: ${default.inputIsPDF})")
        .action   { (_, c) => c.copy(inputIsPDF = true) }

      opt[Unit] ("png")
        .text (s"Input is png files (default: ${!default.inputIsPDF})")
        .action   { (_, c) => c.copy(inputIsPDF = false) }

      opt[String]("convert")
        .text (s"Program to convert from pdf to png (default: ${default.convert})")
        .action { (v, c) => c.copy(convert = v) }

      opt[Int]("density")
        .text (s"Density in dpi for pdf-to-png conversion (default: ${default.dpi})")
        .action { (v, c) => c.copy(dpi = v) }

      opt[Int]('w', "width")
        .text (s"Output image width in pixels (default: ${default.width})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(width = v) }

      opt[Int]('h', "height")
        .text (s"Output image height in pixels (default: ${default.height})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(height = v) }

      opt[Int]("pad")
        .text (s"Padding around each image in pixels (default: ${default.pad})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(pad = v) }

      opt[Int]("tab")
        .text (s"Margin around output image in pixels (default: ${default.tab})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(tab = v) }

      opt[Int]("div")
        .text (s"Reduce the resolution within the packing algorithm (default: ${default.div})")
        .validate { v => if (v >= 1) success else failure("Must be >= 1") }
        .action { (v, c) => c.copy(div = v) }

      opt[Int]('t', "timeout")
        .text (s"Calculation timeout in seconds (default: ${default.timeOut})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(timeOut = v) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    val inputFiles  =
      if (config.inputIsPDF) config.imageDir.children(_.extL == "pdf")
      else                   config.imageDir.children(f => f.extL == "png" || f.extL == "jpg")

    require(inputFiles.nonEmpty, s"No input images detected.")

    val pad2 = config.pad * 2

    val imageFiles = if (!config.inputIsPDF) inputFiles else {
      inputFiles.map { inF =>
        val tempF = File.createTemp(suffix = ".png", deleteOnExit = true)
        import sys.process._
        val cmd = Seq(config.convert, "-density", config.dpi.toString, inF.path, tempF.path)
        println(cmd.mkString(" "))
        cmd.!!
        tempF
      }
    }

    import config.div

    val boxes = imageFiles.map { f =>
      val in      = ImageIO.createImageInputStream(f)
      val reader  = ImageIO.getImageReaders(in).next()
      try {
        reader.setInput(in)
        val w = (reader.getWidth (0) + pad2) / div
        val h = (reader.getHeight(0) + pad2) / div
        new PLP.Box(Symbol(f.name), w /* l */ = h, l /* w */ = w)

      } finally {
        reader.dispose()
      }
    }   .to[SortedSet]
    val p   = PLP.Packer(boxes)
    val res = p.run(w /* l */ = config.height / div, l /* w */ = config.width / div, timeout = config.timeOut * 1000L)

    val imageMap = imageFiles.map(f => Symbol(f.name) -> f).toMap

    res.fold(sys.exit(1)) { plate =>
      val img = new BufferedImage(config.width, config.height, BufferedImage.TYPE_INT_ARGB)
      val g   = img.getGraphics.asInstanceOf[Graphics2D]
      val allowRotation = false
//      val scale = 1.0f

      def draw(b: Locatable, x: Int, y: Int, rot: Boolean = false): Seq[(Graphics2D) => Unit] = b match {
        case Box(name, _ /* l */, _ /* w */) =>
          val fun: Graphics2D => Unit = { g =>
            val imageF  = imageMap(name)
            val img     = ImageIO.read(imageF)
            g.drawImage(img, x + config.pad, y + config.pad, null)
          }
          fun :: Nil
        case Plate(l, w, bs) if rot =>
          bs.zip{ List(
            (b: Locatable) => draw(b, x, y, rot),
            (b: Locatable) => draw(b, x, y + l * div - b.w * div, !rot),
            (b: Locatable) => draw(b, x + w * div - b.w * div, y + l * div - b.l * div, rot),
            (b: Locatable) => draw(b, x + w * div - b.l * div, y, !rot),
            (b: Locatable) => draw(b, x + bs(0).w * div, y + bs(3).w * div, rot))
          }.flatMap{ case (b0, f) => f(b0) }
        case Plate(l, w, bs) =>
          bs.zip{ List(
            (b: Locatable) => draw(b, x, y, rot),
            (b: Locatable) => draw(b, x + l * div - b.w * div, y, !rot && allowRotation),
            (b: Locatable) => draw(b, x + l * div - b.l * div, y + w * div - b.w * div, rot),
            (b: Locatable) => draw(b, x, y + w * div - b.l * div, !rot && allowRotation),
            (b: Locatable) => draw(b, x + bs(3).w * div, y + bs(0).w * div, rot))
          }.flatMap{ case (b0, f) => f(b0) }
      }

      val cmd = draw(plate, config.tab, config.tab)
      println("Drawing...")
      cmd.foreach(_.apply(g))

      g.dispose()
      val fmt = if (config.output.extL == "jpg") "jpg" else "png"
      println("Writing...")
      config.output.delete()
      ImageIO.write(img, fmt, config.output)
      println("Done.")
      sys.exit()
    }
  }
}
