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

  val RED_RANGE = (190, 255)
  val GREEN_RANGE = (0, 100)
  val BLUE_RANGE = (0, 100)

  val PADDING_SNIPPET = 150
  val MINIMAL_SNIPPET_HEIGHT = 300

  val SNIPPET_DIR = "../delta_snippets/"

  val filterDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir, name).isDirectory
  }

  val snippetDir: File = new File(SNIPPET_DIR)

  snippetDir.listFiles(filterDirectories).par.foreach(methodDir => {
    methodDir.listFiles(filterDirectories).par.foreach(pdfDir => {
      createSnippet(pdfDir)
    })
  })


  def createSnippet(directory: File): Unit = {
    logger.debug(s"Working directory: $directory")
    try {
      directory.listFiles(filterDirectories).par.foreach(permutationDir =>
        permutationDir.listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = name.endsWith(".png")
        }).par.foreach(image => {

          val pngImage = ImageIO.read(image)

          val redCoordsSnippets = for (x <- 0 until pngImage.getWidth; y <- 0 until pngImage.getHeight; if isColorInRange(new Color(pngImage.getRGB(x, y)))) yield {
            new Point2D.Double(x, y)
          }

          extractAndGenerateImage(image, redCoordsSnippets.toList)

        })
      )
    } catch {
      case e: Exception => logger.error("Error: ", e)
    }

  }


  def extractAndGenerateImage(pngImage: File, redCoords: List[Point2D]): Boolean = {

    if (redCoords.nonEmpty) {
      val inputImage = ImageIO.read(pngImage)
      val (startY: Int, endY: Int) = extractImageBoundaries(redCoords, inputImage.getHeight)
      val snippetHeight = endY - startY

      val imageWidth = inputImage.getWidth()

      val snippetImage = new BufferedImage(imageWidth, snippetHeight, BufferedImage.TYPE_INT_RGB)
      for (w <- 0 until imageWidth) {
        for (h <- 0 until snippetHeight) {
          try {
            snippetImage.setRGB(w, h, new Color(inputImage.getRGB(w, startY + h)).getRGB)
          } catch {
            case e: Exception => snippetImage.setRGB(w, h, Color.BLACK.getRGB)
          }
        }
      }

      val storeSnippetPath = pngImage.getParentFile.getParentFile.getParentFile

      ImageIO.write(snippetImage, "png", new File(storeSnippetPath + "/" + pngImage.getName))
      logger.debug(s"Snippet successfully written: ${storeSnippetPath + "/" + pngImage.getName}")
      true
    } else {
      logger.error(s"Cannot create snippet. No highlight found in file: ${pngImage.getName}")
      false
    }
  }

  def extractImageBoundaries(coordsYellow: List[Point2D], maxHeight: Int): (Int, Int) = {
    val minYellow = coordsYellow.minBy(_.getY)

    val maxYellow = coordsYellow.maxBy(_.getY)

    val startY = if (minYellow.getY - PADDING_SNIPPET >= 0) {
      minYellow.getY - PADDING_SNIPPET
    } else {
      0
    }
    val endY = if (maxYellow.getY + PADDING_SNIPPET <= maxHeight) {
      maxYellow.getY + PADDING_SNIPPET
    } else {
      maxHeight
    }

    checkMinimalBoundaries(startY.toInt, endY.toInt, maxHeight)
  }

  def checkMinimalBoundaries(startY: Int, endY: Int, maxImageHeight: Int): (Int, Int) = {
    var minY = startY
    var maxY = endY
    val originalHeight = delta(maxY, minY)
    if (originalHeight < MINIMAL_SNIPPET_HEIGHT) {

      val deltaHeight = (MINIMAL_SNIPPET_HEIGHT - originalHeight) / 2

      if (minY - deltaHeight > 0) {
        minY = minY - deltaHeight
      } else {
        minY = 0
      }

      if (maxY + deltaHeight < maxImageHeight) {
        maxY = maxY + deltaHeight
      } else {
        maxY = maxImageHeight
      }
    }
    (minY, maxY)
  }

  def delta(x: Int, y: Int): Int = {
    Math.abs(x - y)
  }

  def isColorInRange(color1: Color): Boolean = {
    color1.getRed <= RED_RANGE._2 &&
      color1.getRed >= RED_RANGE._1 &&
      color1.getGreen <= GREEN_RANGE._2 &&
      color1.getGreen >= GREEN_RANGE._1 &&
      color1.getBlue <= BLUE_RANGE._2 &&
      color1.getBlue >= BLUE_RANGE._1
  }

}
