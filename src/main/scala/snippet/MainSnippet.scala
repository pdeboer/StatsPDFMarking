package snippet

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, FilenameFilter}
import javax.imageio.ImageIO

import com.typesafe.scalalogging.LazyLogging

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging {

  val COLOR_TOLERANCE = 3

  val YELLOW = (255, 255,127)
  val GREEN = (127, 255, 127)

  val PADDING_SNIPPET = 10
  val MINIMAL_SNIPPET_HEIGHT = 300

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

      var yellowCoords = List.empty[Coords]
      var greenCoords = List.empty[Coords]

      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val color = new Color(inputImage.getRGB(x, y))

          if (isSameColor(color, YELLOW)) {
            yellowCoords ::= Coords(x,y)
          } else if (isSameColor(color, GREEN)) {
            greenCoords ::= Coords(x,y)
          }
        }
      }

      if (greenCoords.nonEmpty && yellowCoords.nonEmpty) {
        val (startY: Int, endY: Int) = extractImageBoundaries(yellowCoords, greenCoords, height)
        val snippetHeight = endY - startY

        val snippetImage = new BufferedImage(width, snippetHeight, BufferedImage.TYPE_INT_RGB)
        for (w <- 0 until width) {
          for(h <- 0 until snippetHeight){
            snippetImage.setRGB(w, h, new Color(inputImage.getRGB(w, startY + h)).getRGB)
          }
        }
        ImageIO.write(snippetImage, "png", new File(SNIPPET_DIR+pngImage.getName))
        logger.debug("Snippet successfully written")
      }
    })
  })

  def extractImageBoundaries(coordsYellow: List[Coords], coordsGreen: List[Coords], maxHeight: Int): (Int, Int) = {
    val minGreen = coordsGreen.minBy(_.y)
    val minYellow = coordsYellow.minBy(_.y)

    val maxGreen = coordsGreen.maxBy(_.y)
    val maxYellow = coordsYellow.maxBy(_.y)

    val startY = Math.min(minYellow.y,minGreen.y) - PADDING_SNIPPET
    val endY = Math.max(maxYellow.y, maxGreen.y) + PADDING_SNIPPET

    checkMinimalBoundaries(startY, endY, maxHeight)
  }

  def checkMinimalBoundaries(startY: Int, endY: Int, maxHeight: Int) : (Int, Int) = {
    var minY = startY
    var maxY = endY
    val originalHeight = maxY-minY
    if(originalHeight < MINIMAL_SNIPPET_HEIGHT) {

      val deltaHeight = MINIMAL_SNIPPET_HEIGHT-originalHeight

      if(minY-(deltaHeight/2)>0){
        minY = minY - (deltaHeight/2)
      } else {
        minY = 0
      }

      if(maxY+(deltaHeight/2) < maxHeight){
        maxY = maxY + (deltaHeight/2)
      } else {
        maxY = maxHeight
      }
    }
    (minY, maxY)
  }

  def delta(x: Int, y: Int) : Int= {
    Math.abs(x-y)
  }

  def isSameColor(color1: Color, color: (Int, Int, Int)): Boolean = {
    delta(color1.getRed, color._1) < COLOR_TOLERANCE &&
      delta(color1.getGreen, color._2) < COLOR_TOLERANCE &&
      delta(color1.getBlue, color._3) < COLOR_TOLERANCE
  }

}
