package de.sciss.plp

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.file._

object PackImages {
  final case class Config(
                           dpi        : Int     = 72,
                           convert    : String  = "convert",
                           imageDir   : File    = file("images"),
                           output     : File    = file("output.png"),
                           inputIsPDF : Boolean = false,
                           width      : Int     = 400,
                           height     : Int     = 400,
                           timeOut    : Int     = 60
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

      opt[Int]('t', "timeout")
        .text (s"Calculation timeout in seconds (default: ${default.timeOut})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(timeOut = v) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    val imageFiles  =
      if (config.inputIsPDF) config.imageDir.children(_.extL == "pdf")
      else                   config.imageDir.children(f => f.extL == "png" || f.extL == "jpg")

    require(imageFiles.nonEmpty, s"No input images detected.")

    val p   = PLP.Packer(???)
    val res = p.run(l = config.height, w = config.width, timeout = config.timeOut * 1000L)
    res.fold(sys.exit(1)) { plate =>
      val img = new BufferedImage(config.width, config.height, BufferedImage.TYPE_INT_ARGB)
      val g   = img.getGraphics

      val boxes   = PLP.Locatable.flatten(plate)
      boxes.foreach { box =>
        box.l
      }

      g.dispose()
      val fmt = if (config.output.extL == "jpg") "jpg" else "png"
      ImageIO.write(img, fmt, config.output)
    }
  }
}
