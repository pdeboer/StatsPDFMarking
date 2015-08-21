package snippet

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.io.{File, FilenameFilter}
import javax.imageio.ImageIO

import com.typesafe.scalalogging.LazyLogging
import org.codehaus.plexus.util.FileUtils

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging {

  val YELLOW_RANGES= List[(Int, Int)]((200, 255), (200, 255), (100, 150))
  val GREEN_RANGES= List[(Int, Int)]((100, 150), (200, 255), (100, 150))

  val PADDING_SNIPPET = 200
  val MINIMAL_SNIPPET_HEIGHT = 300

  val SNIPPET_DIR = "../merge_method_snippets/"

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


  def createSnippet(pngImage: File): String = {
    logger.debug(s"Working file: $pngImage")
    try {

      val inputImage = ImageIO.read(pngImage)

      val width = inputImage.getWidth
      val height = inputImage.getHeight

      var yellowCoords = List.empty[Point2D]
      var greenCoords = List.empty[Point2D]

      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val color = new Color(inputImage.getRGB(x, y))
          if (isSameColor(color, YELLOW_RANGES)) {
            yellowCoords ::= new Point2D.Double(x, y)
          } else if (isSameColor(color, GREEN_RANGES)) {
            greenCoords ::= new Point2D.Double(x, y)
          }
        }
      }

      extractAndGenerateImage(pngImage, yellowCoords, greenCoords)
      
    }catch {
      case e: Exception => {
        logger.error("Error: ", e)
        ""
      }
    }

  }

  def extractAndGenerateImage(pngImage: File, yellowCoords: List[Point2D], greenCoords: List[Point2D]): String = {

    if (greenCoords.nonEmpty && yellowCoords.nonEmpty) {

      //multiple yellow, single green

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
      val methodOnTop: Boolean = if(yellowCoords.map(y => (Math.abs(greenCoords.minBy(_.getY).getY - y.getY), y)).minBy(_._1)._2.getY < greenCoords.minBy(_.getY).getY){
          true
        } else {
          false
        }

      var suffix = "methodOnTop"
      if(!methodOnTop){
        suffix = "prerequisiteOnTop"
      }

      val storeSnippetPath = pngImage.getParentFile.getPath

      val snippetFile = new File(storeSnippetPath + "/" + pngImage.getName.substring(0, pngImage.getName.indexOf(".png"))+"-"+suffix+".png")

      ImageIO.write(snippetImage, "png", snippetFile)
      logger.debug(s"Snippet successfully written: ${storeSnippetPath + "/" + pngImage.getName.substring(0, pngImage.getName.indexOf(".png"))+"-"+suffix+".png"}")
      snippetFile.getPath
    } else {
      logger.error(s"Cannot create snippet. No highlight found in file: ${pngImage.getName}")
      new File("../errors_cutting_snippets").mkdir()
      val snippet = new File("../errors_cutting_snippets/"+pngImage.getName)
      try{
        FileUtils.copyFile(pngImage, snippet)
      }catch {
        case e: Exception => logger.error(s"Cannot copy file $pngImage to ../errors_cutting_snippets/ directory!", e)
      }
      ""
    }
  }

  def extractImageBoundaries(coordsYellow: List[Point2D], coordsGreen: List[Point2D], maxHeight: Int): (Int, Int) = {

    val minGreen = coordsGreen.minBy(_.getY)
    val minYellow = coordsYellow.map(y => (Math.abs(minGreen.getY - y.getY), y)).minBy(_._1)._2

    val startY = Math.max(0, Math.min(minYellow.getY,minGreen.getY) - PADDING_SNIPPET)
    val endY = Math.min(Math.max(minYellow.getY, minGreen.getY) + PADDING_SNIPPET, maxHeight)

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
