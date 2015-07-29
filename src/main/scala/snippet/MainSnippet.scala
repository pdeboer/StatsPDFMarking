package snippet

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.{File, FilenameFilter}
import javax.imageio.ImageIO

import com.typesafe.scalalogging.LazyLogging

import scala.sys.process._

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging {

  val COLOR_TOLERANCE = 3

  val YELLOW = new Color(255, 255,127)
  val GREEN = new Color(127, 255, 127)

  val PADDING_SNIPPET = 10
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

  val allSubDirFiles = outputSubDirectories.map (directory => {

    logger.debug(s"Working directory: $directory")
    var yellowCoords = List.empty[(Int, Point2D)]
    var greenCoords = List.empty[(Int, Point2D)]
    var smallSnippet = false

    new File(OUTPUT_DIR + directory).listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = {
        name.endsWith(".png")
      }
    }).foreach(pngImage => {

      val inputImage = ImageIO.read(pngImage)
      val width = inputImage.getWidth
      val height = inputImage.getHeight

      val pageNr = pngImage.getName.substring(pngImage.getName.indexOf("-")+1, pngImage.getName.indexOf(".png")).toInt
      var yellowSinglePage = List.empty[Point2D]
      var greenSinglePage = List.empty[Point2D]

      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val color = new Color(inputImage.getRGB(x, y))
          if (isSameColor(color, YELLOW)) {
            yellowCoords ::= (pageNr, new Point2D.Double(x,y))
            yellowSinglePage ::= new Point2D.Double(x,y)
          } else if (isSameColor(color, GREEN)) {
            greenCoords ::= (pageNr, new Point2D.Double(x,y))
            greenSinglePage ::= new Point2D.Double(x,y)
          }
        }
      }
      // If there are green and yellow matches on same image, create snippet
      smallSnippet = extractAndStoreImage(pngImage.getName, inputImage, yellowSinglePage, greenSinglePage)
    })

    // Create big snippets if there exists not a small one
    if(!smallSnippet){
      try {
        val pageFirstMatch: Int = Math.min(yellowCoords.minBy(_._1)._1, greenCoords.minBy(_._1)._1)
        val pageLastMatch: Int = Math.max(yellowCoords.maxBy(_._1)._1, greenCoords.maxBy(_._1)._1)

        var allPngs = ""
        new File(OUTPUT_DIR + directory).listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = {
            name.endsWith(".png")
          }
        }).foreach(img => {
          if(img.getName.endsWith("-"+pageFirstMatch+".png")) {
            val fileName = img.getName
            for (i <- pageFirstMatch to pageLastMatch) {
              allPngs += " " + OUTPUT_DIR + directory + "/" + fileName.substring(0, fileName.indexOf("-")+1)+i+".png"
            }
          }
        })

        logger.debug("Creating multiple page snippet")
        ("/opt/local/bin/convert" + allPngs + " -append " + SNIPPET_DIR + "/"+directory+"-"+pageFirstMatch+".png").!!
      }
      catch{
        case e: Exception => logger.error("Something not good happened", e)
      }
    }
  })

  def extractAndStoreImage(pngFileName: String, inputImage: BufferedImage, yellowCoords: List[Point2D], greenCoords: List[Point2D]): Boolean = {
    if (greenCoords.nonEmpty && yellowCoords.nonEmpty) {
      val (startY: Int, endY: Int) = extractImageBoundaries(yellowCoords, greenCoords, inputImage.getHeight)
      val snippetHeight = endY - startY

      val imageWidth = inputImage.getWidth()

      val snippetImage = new BufferedImage(imageWidth, snippetHeight, BufferedImage.TYPE_INT_RGB)
      for (w <- 0 until imageWidth) {
        for (h <- 0 until snippetHeight) {
          snippetImage.setRGB(w, h, new Color(inputImage.getRGB(w, startY + h)).getRGB)
        }
      }
      ImageIO.write(snippetImage, "png", new File(SNIPPET_DIR + pngFileName))
      logger.debug("Snippet successfully written")
      true
    } else {
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
