package snippet

import java.awt.image.BufferedImage
import java.io.{File, FilenameFilter}
import javax.imageio.ImageIO

import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging {

  val COLOR_TOLERANCE = 3

  val YELLOW = (255, 255,127)
  val GREEN = (127, 255, 127)

  val PADDING_SNIPPET = 10

  val SNIPPET_DIR = "snippets/"
  val OUTPUT_DIR = "output/"

  new File(SNIPPET_DIR).mkdirs()
  new File(SNIPPET_DIR).listFiles().foreach(f => { f.delete() })

  val outputDir: File = new File(OUTPUT_DIR)

  val outputSubDirectories: List[String] = outputDir.list(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = {
      new File(dir, name).isDirectory
    }
  }).toList


  outputSubDirectories.par.foreach(directory => {
    new File(OUTPUT_DIR + directory).listFiles().par.foreach(pngImage => {
      val inputImage = ImageIO.read(pngImage)
      val width = inputImage.getWidth
      val height = inputImage.getHeight

      val yellowCoords = new mutable.MutableList[Coords]()
      val greenCoords = new mutable.MutableList[Coords]()

      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val color = (inputImage.getRGB(x, y) & 0xffffff)
          val red = (color & 0xff0000) / 65536
          val green = (color & 0xff00) / 256
          val blue = (color & 0xff)

          if (isColor(red, green, blue, YELLOW)) {
            yellowCoords += Coords(x,y)
          } else if (isColor(red, green, blue, GREEN)) {
            greenCoords += Coords(x,y)
          }
        }
      }

      if (greenCoords.nonEmpty && yellowCoords.nonEmpty) {
        val (startY: Int, endY: Int) = extractImageBoundaries(yellowCoords.toList, greenCoords.toList)
        val snippetHeight = endY - startY

        val snippetImage = new BufferedImage(width, snippetHeight, BufferedImage.TYPE_INT_RGB)
        for (w <- 0 until width) {
          for(h <- 0 until snippetHeight){
            snippetImage.setRGB(w, h, inputImage.getRGB(w, startY + h) & 0xffffff)
          }
        }
        ImageIO.write(snippetImage, "png", new File(SNIPPET_DIR+pngImage.getName))
        logger.debug("Snippet successfully written")
      }
    })
  })

  def extractImageBoundaries(coordsYellow: List[Coords], coordsGreen: List[Coords]): (Int, Int) = {
    val minGreen = coordsGreen.minBy(_.y)
    val minYellow = coordsYellow.minBy(_.y)

    val maxGreen = coordsGreen.maxBy(_.y)
    val maxYellow = coordsYellow.maxBy(_.y)

    val startY = Math.min(minYellow.y,minGreen.y) - PADDING_SNIPPET
    val endY = Math.max(maxYellow.y, maxGreen.y) + PADDING_SNIPPET

    (startY, endY)
  }

  def getAbsDifference(x: Int, y: Int) : Int= {
    Math.abs(x-y)
  }

  def isColor(r: Int, g: Int, b: Int, color: (Int, Int, Int)): Boolean = {
    getAbsDifference(r, color._1) < COLOR_TOLERANCE &&
      getAbsDifference(g, color._2) < COLOR_TOLERANCE &&
      getAbsDifference(b, color._3) < COLOR_TOLERANCE
  }

}
