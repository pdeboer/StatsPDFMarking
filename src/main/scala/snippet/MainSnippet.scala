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

  val COLOR_TOLERANCE = 15

  val YELLOW = new Color(255, 255,127)
  val GREEN = new Color(127, 255, 127)

  val PADDING_SNIPPET = 200
  val MINIMAL_SNIPPET_HEIGHT = 300

  val SNIPPET_DIR = "../snippets/"
  val OUTPUT_DIR = "../output/"

  new File(SNIPPET_DIR).mkdir()
  new File(SNIPPET_DIR).listFiles().foreach(f => {
    if(f.isDirectory) {
      f.listFiles().foreach(f1 =>
        f1.delete()
      )}
    f.delete()
  })

  val outputDir: File = new File(OUTPUT_DIR)

  val methodsDirectories: List[File] = outputDir.listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir, name).isDirectory
  }).toList

  var permutationsDirectories : List[File] = List.empty[File]

  methodsDirectories.foreach(methodDirectory => {
    permutationsDirectories = permutationsDirectories ::: methodDirectory.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = new File(dir, name).isDirectory
    }).toList
  })

  permutationsDirectories.map (directory => {

    logger.debug(s"Working directory: $directory")
    try {
      directory.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = {
          name.endsWith(".png")
        }
      }).foreach(pngImage => {

        logger.debug(s"Found file: ${pngImage.getName}")

        val inputImage = ImageIO.read(pngImage)

        val width = inputImage.getWidth
        val height = inputImage.getHeight

        var yellowCoords = List.empty[Point2D]
        var greenCoords = List.empty[Point2D]

        for (x <- 0 until width) {
          for (y <- 0 until height) {
            val color = new Color(inputImage.getRGB(x, y))
            if (isSameColor(color, YELLOW)) {
              yellowCoords ::= new Point2D.Double(x, y)
            } else if (isSameColor(color, GREEN)) {
              greenCoords ::= new Point2D.Double(x, y)
            }
          }
        }

        extractAndGenerateImage(pngImage, yellowCoords, greenCoords)

      })
    } catch {
      case e: Exception => logger.error("Error: ", e)
    }
  })


  def extractAndGenerateImage(pngImage: File, yellowCoords: List[Point2D], greenCoords: List[Point2D]): Boolean = {

    if (greenCoords.nonEmpty && yellowCoords.nonEmpty) {
      val inputImage = ImageIO.read(pngImage)
      val (startY: Int, endY: Int) = extractImageBoundaries(yellowCoords, greenCoords, inputImage.getHeight)
      val snippetHeight = endY - startY

      val imageWidth = inputImage.getWidth()

      val snippetImage = new BufferedImage(imageWidth, snippetHeight, BufferedImage.TYPE_INT_RGB)
      for (w <- 0 until imageWidth) {
        for (h <- 0 until snippetHeight) {
          snippetImage.setRGB(w, h, new Color(inputImage.getRGB(w, startY + h)).getRGB)
        }
      }
      val methodOnTop: Boolean = if(yellowCoords.minBy(_.getY).getY < greenCoords.minBy(_.getY).getY){ true } else {false}
      var suffix = "methodOnTop"
      if(!methodOnTop){
        suffix = "prerequisiteOnTop"
      }

      val methodName = pngImage.getParentFile.getParentFile.getName
      if(!new File(SNIPPET_DIR + methodName).exists()) {
        new File(SNIPPET_DIR + methodName).mkdir()
      }

      ImageIO.write(snippetImage, "png", new File(SNIPPET_DIR + methodName + "/" + pngImage.getName.substring(0, pngImage.getName.indexOf(".png"))+"-"+suffix+".png"))
      logger.debug(s"Snippet successfully written: ${SNIPPET_DIR + methodName + "/" + pngImage.getName.substring(0, pngImage.getName.indexOf(".png"))+"-"+suffix+".png"}")
      true
    } else {
      logger.error(s"Cannot create snippet. No highlight found in file: ${pngImage.getName}")
      false
    }
  }

  def extractImageBoundaries(coordsYellow: List[Point2D], coordsGreen: List[Point2D], maxHeight: Int): (Int, Int) = {
    val minGreen = coordsGreen.minBy(_.getY)
    val minYellow = coordsYellow.minBy(_.getY)

    val maxGreen = coordsGreen.maxBy(_.getY)
    val maxYellow = coordsYellow.maxBy(_.getY)

    val startY = Math.min(minYellow.getY,minGreen.getY) - PADDING_SNIPPET
    val endY = Math.max(maxYellow.getY, maxGreen.getY) + PADDING_SNIPPET

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
