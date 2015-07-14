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

  val snippetsDir = "snippets/"

  new File(snippetsDir).mkdirs()
  new File(snippetsDir).listFiles().foreach(f => f.delete())

  val outputDir: File = new File("output")

  val directories: List[String] = outputDir.list(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = {
      new File(dir, name).isDirectory
    }
  }).toList


  directories.par.foreach(dir => {

    var coordsYellow = new mutable.HashMap[Int, Int]()
    var coordsGreen = new mutable.HashMap[Int, Int]()

    new File("output/" + dir).listFiles().par.foreach(pngFile => {
      logger.debug("Searching for Yellow or Green highlight on: " + pngFile.getName)

      val in = ImageIO.read(pngFile)
      val width = in.getWidth
      val height = in.getHeight


      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val color = (in.getRGB(x, y) & 0xffffff)
          val red = (color & 0xff0000) / 65536
          val green = (color & 0xff00) / 256
          val blue = (color & 0xff)
          if (getDifference(red, 255) < 5 && getDifference(green, 255) < 5 && getDifference(blue, 127) < 5) {
            coordsYellow += (x -> y)

          } else if (getDifference(red, 127) < 5 && getDifference(green, 255) < 5 && getDifference(blue, 127) < 5) {
            coordsGreen += (x -> y)
          }
        }
      }

      // Analyze coords maps
      if (coordsGreen.nonEmpty && coordsYellow.nonEmpty) {

        val minGreenY = coordsGreen.minBy(_._2)._2
        val minYellowY = coordsYellow.minBy(_._2)._2

        //Calculate rows
        val startRow = Math.min(minYellowY,minGreenY)-20
        val endRow = Math.max(minYellowY, minGreenY)+20

        logger.debug("Min Green: " + minGreenY)
        logger.debug("Min Yellow: " + minYellowY)

        val img = new BufferedImage(width, endRow-startRow, BufferedImage.TYPE_INT_RGB)

        for (roww <- 0 until width) {
          for(coll <- 0 until endRow-startRow){
            img.setRGB(roww, coll, in.getRGB(roww, startRow + coll) & 0xffffff)
          }
        }

        ImageIO.write(img, "png", new File("snippets/"+pngFile.getName))
        logger.debug("End image write")
        coordsGreen = mutable.HashMap.empty
        coordsYellow = mutable.HashMap.empty
      }

    })

  })

  def getDifference(x: Int, y: Int) = {
    Math.abs(x-y)
  }

}
