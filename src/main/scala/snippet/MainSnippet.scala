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
    logger.debug("Searching for Yellow or Green highlight on: " + pngFile.getName)
    try {
      val in = new PngReader(pngFile)

      val coordsYellow = new mutable.HashMap[Int, Int]()
      val coordsGreen = new mutable.HashMap[Int, Int]()
      val channels = in.imgInfo.channels

      for (row <- 0 to in.imgInfo.rows) {
        val l1 = in.readRow()
        // get all the pixel values on the row
        val scanline = l1.asInstanceOf[ImageLineInt].getScanline
        for (col <- 0 to in.imgInfo.cols-3) {

          val r = scanline(col * channels)
          val g = scanline((col * channels) + 1)
          val b = scanline((col * channels) + 2)

          if (r == 253 && g == 250 && b == 150) {

            logger.debug("YELLOW " + pngFile.getName)
            val out = new PngWriter(new File("snippets/"+pngFile.getName), in.imgInfo, true)
            out.copyChunksFrom(in.getChunksList(), ChunkCopyBehaviour.COPY_ALL)
            out.getMetadata().setText(PngChunkTextVar.KEY_Description, "Identify highlighted text")
            for (roww <- 0 to in.imgInfo.rows) {
              out.writeRow(in.readRow(roww))
            }
            out.end()
            logger.debug("End image write")

          } else if (r == 180 && g == 251 && b == 150) {

            logger.debug("GREEN " + pngFile.getName)
            val out = new PngWriter(new File("snippets/"+pngFile.getName), in.imgInfo, true)
            out.copyChunksFrom(in.getChunksList(), ChunkCopyBehaviour.COPY_ALL)
            out.getMetadata().setText(PngChunkTextVar.KEY_Description, "Identify highlighted text")
            for (roww <- 0 to in.imgInfo.rows) {
              out.writeRow(in.readRow(roww))
            }
            out.end()
            logger.debug("End image write")

          }
        }
      }
      in.end()
    } catch {
      case e: Exception => {
        logger.error("An error occurred while analyzing the png pngFile")
        e.printStackTrace()
      }
    }
  })

  def getDifference(color:Int, color2: Int): Int = {
    Math.abs(color - color2)
  }

}
