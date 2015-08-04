package snippet

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.{File, FilenameFilter}
import javax.imageio.ImageIO

import com.typesafe.scalalogging.LazyLogging

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging {

  val COLOR_TOLERANCE = 3

  val YELLOW = new Color(247, 255, 132)

  val CONVERT_APP = "/opt/local/bin/convert "

  val PADDING_SNIPPET = 200
  val MINIMAL_SNIPPET_HEIGHT = 300

  val SNIPPET_DIR = "../snippets/"
  val OUTPUT_DIR = "../output/"

  new File(SNIPPET_DIR).mkdirs()
  new File(SNIPPET_DIR).listFiles().foreach(f => { f.delete() })

  val outputDir: File = new File(OUTPUT_DIR)

  val outputSubDirectories: List[String] = outputDir.list(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = {
      new File(dir, name).isDirectory
    }
  }).toList

  outputSubDirectories.par.map (directory => {

    logger.debug(s"Working directory: $directory")

    try {
      new File(OUTPUT_DIR + directory).listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = {
          name.endsWith(".png")
        }
      }).foreach(pngImage => {
        val image = ImageIO.read(pngImage)
        var yellowCoordsSnippet = List.empty[Point2D]

          for (x <- 0 until image.getWidth) {
            for (y <- 0 until image.getHeight) {
              val color = new Color(image.getRGB(x, y))
              if (isSameColor(color, YELLOW)) {
                yellowCoordsSnippet ::= new Point2D.Double(x, y)
              }
            }
          }


        extractAndGenerateImage(pngImage, yellowCoordsSnippet)
      })
    } catch {
      case e: Exception => {
        logger.error("Error", e)
      }
    }
  })


  def extractAndGenerateImage(pngImage: File, yellowCoords: List[Point2D]): Boolean = {

    if (yellowCoords.nonEmpty) {
      val inputImage = ImageIO.read(pngImage)
      val (startY: Int, endY: Int) = extractImageBoundaries(yellowCoords, inputImage.getHeight)
      val snippetHeight = endY - startY

      val imageWidth = inputImage.getWidth()

      val snippetImage = new BufferedImage(imageWidth, snippetHeight, BufferedImage.TYPE_INT_RGB)
      for (w <- 0 until imageWidth) {
        for (h <- 0 until snippetHeight) {
          snippetImage.setRGB(w, h, new Color(inputImage.getRGB(w, startY + h)).getRGB)
        }
      }

      ImageIO.write(snippetImage, "png", new File(SNIPPET_DIR + pngImage.getName))
      logger.debug(s"Snippet successfully written: ${SNIPPET_DIR + pngImage.getName}")
      true
    } else {
      false
    }
  }

  def extractImageBoundaries(coordsYellow: List[Point2D], maxHeight: Int): (Int, Int) = {
    val minYellow = coordsYellow.minBy(_.getY)

    val maxYellow = coordsYellow.maxBy(_.getY)

    val startY = minYellow.getY - PADDING_SNIPPET
    val endY = maxYellow.getY + PADDING_SNIPPET

    checkMinimalBoundaries(startY.toInt, endY.toInt, maxHeight)
  }

  def checkMinimalBoundaries(startY: Int, endY: Int, maxImageHeight: Int): (Int, Int) = {
    var minY = startY
    var maxY = endY
    val originalHeight = maxY-minY
    if(originalHeight < MINIMAL_SNIPPET_HEIGHT) {

      val deltaHeight = (MINIMAL_SNIPPET_HEIGHT-originalHeight)/2

      if(minY-deltaHeight >0){
        minY = minY - deltaHeight
      } else {
        minY = 0
      }

      if(maxY+deltaHeight < maxImageHeight){
        maxY = maxY + deltaHeight
      } else {
        maxY = maxImageHeight
      }
    }
    (minY, maxY)
  }

  def delta(x: Int, y: Int) : Int= {
    Math.abs(x-y)
  }

  def isSameColor(color1: Color, color2: Color): Boolean = {
    delta(color1.getRed, color2.getRed) < COLOR_TOLERANCE &&
      delta(color1.getGreen, color2.getGreen) < COLOR_TOLERANCE &&
      delta(color1.getBlue, color2.getBlue) < COLOR_TOLERANCE
  }

}
