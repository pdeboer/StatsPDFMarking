package snippet

import java.io.File

import ar.com.hjg.pngj._
import ar.com.hjg.pngj.chunks.{ChunkCopyBehaviour, PngChunkTextVar}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging{

  val snippetsDir = "snippets/"

  new File(snippetsDir).mkdirs()
  new File(snippetsDir).listFiles().foreach(f => f.delete())

  val pngFiles : List[File] = new File("output").listFiles().filter(f => f.getName.endsWith(".png")).toList

  pngFiles.par.foreach(pngFile => {
    //logger.debug("Searching for Yellow or Green highlight on: " + pngFile.getName)
    try {
      val in = new PngReader(pngFile)

      val coordsYellow = new mutable.HashMap[Int, Int]()
      val coordsGreen = new mutable.HashMap[Int, Int]()
      val channels = in.imgInfo.channels
      if(in.imgInfo.indexed) {
        val palette = in.getMetadata().getPLTE()
        val trns = in.getMetadata.getTRNS()

        for (row <- 0 until in.imgInfo.rows) {
          val l1 = in.readRow()
          val indexPalette = ImageLineHelper.palette2rgb(l1.asInstanceOf[ImageLineInt], palette, trns, null)
          // get all the pixel values on the row
          //val scanline = l1.asInstanceOf[ImageLineInt].getScanline
          for (col <- 0 until in.imgInfo.cols - 4) {

            val r = indexPalette(col * channels)
            val g = indexPalette((col * channels) + 1)
            val b = indexPalette((col * channels) + 2)

            if (palette != null) {
              if (getDifference(r, 255) < 3 && getDifference(g, 255) < 3 && getDifference(b, 120) < 3) {
                coordsYellow += (row -> col)
                //logger.debug("File: " + pngFile.getName + "; row: " + row + ", col:" + col)
              } else if (getDifference(r, 119) < 3 && getDifference(g, 255) < 3 && getDifference(b, 120) < 3) {
                coordsGreen += (row -> col)
                //logger.debug("File: " + pngFile.getName + "; row: " + row + ", col:" + col)
              }
            }
          }
        }
        in.end()

        // Analyze coords maps
        if (coordsGreen.nonEmpty && coordsYellow.nonEmpty) {

          val input = new PngReader(pngFile)

          val minGreenY = coordsGreen.minBy(_._2)
          val minYellowY = coordsYellow.minBy(_._2)

          logger.debug("Min Green: " + minGreenY)
          logger.debug("Min Yellow: " + minYellowY)

          val out = new PngWriter(new File("snippets/" + pngFile.getName), input.imgInfo, true)
          out.copyChunksFrom(input.getChunksList(), ChunkCopyBehaviour.COPY_ALL)
          out.getMetadata().setText(PngChunkTextVar.KEY_Description, "Identify highlighted text")

          for (roww <- 0 until input.imgInfo.rows) {
            out.writeRow(input.readRow())
          }

          out.end()
          logger.debug("End image write")

          input.end()
        }
      } else {
        logger.error("Image: " + pngFile.getName + " not indexed")
        in.end()
      }


    } catch {
      case e: Exception => {
        logger.error("An error occurred while analyzing the png pngFile")
        e.printStackTrace()
      }
    }
  })

  def getDifference(color:Int, color2: Int): Double = {
    Math.abs(color - color2)
  }

}
