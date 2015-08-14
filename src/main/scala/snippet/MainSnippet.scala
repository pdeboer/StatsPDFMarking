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

  val RED_RANGES= List[(Int, Int)]((190, 255), (0, 100), (0, 100))

  val PADDING_SNIPPET = 200
  val MINIMAL_SNIPPET_HEIGHT = 300

  val SNIPPET_DIR = "../delta_snippets/"

  val filterDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir, name).isDirectory
  }

  val snippetDir: File = new File(SNIPPET_DIR)

  val allBigSnippets: List[File] = snippetDir.listFiles(filterDirectories).flatMap(yearDirs => {
    yearDirs.listFiles(filterDirectories).flatMap(methodDirs => {
      methodDirs.listFiles(filterDirectories).flatMap(pdfDirs => {
        pdfDirs.listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = name.endsWith(".png")
        }).map(bigSnippet => bigSnippet)
      }).toList
    }).toList
  }).toList

  logger.debug(s"Found: ${allBigSnippets.length} png files")
  
  allBigSnippets.par.foreach(createSnippet(_))


  def createSnippet(pngImage: File): Unit = {
    logger.debug(s"Working file: $pngImage")
    try {

      val inputImage = ImageIO.read(pngImage)

      val width = inputImage.getWidth
      val height = inputImage.getHeight

      val redCoords = for (x <- 0 until width; y <- 0 until height; if(isSameColor(new Color(inputImage.getRGB(x, y)), RED_RANGES)))  yield {
        new Point2D.Double(x, y)
      }

      extractAndGenerateImage(pngImage, redCoords.toList)
      
    }catch {
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
          snippetImage.setRGB(w, h, new Color(inputImage.getRGB(w, startY + h)).getRGB)
        }
      }

      val storeSnippetPath = pngImage.getParentFile.getPath

      ImageIO.write(snippetImage, "png", new File(storeSnippetPath + "/" + pngImage.getName.substring(0, pngImage.getName.indexOf(".png"))+".png"))
      logger.debug(s"Snippet successfully written: ${storeSnippetPath + "/" + pngImage.getName.substring(0, pngImage.getName.indexOf(".png"))+".png"}")
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

  def isSameColor(color1: Color, color2: List[(Int, Int)]): Boolean = {
    color1.getRed <= color2.head._2 &&
      color1.getRed >= color2.head._1 &&
      color1.getGreen <= color2(1)._2 &&
      color1.getGreen >= color2(1)._1 &&
      color1.getBlue <= color2.last._2 &&
      color1.getBlue >= color2.last._1
  }

}
