package snippet

import java.awt.Color
import java.awt.image.ColorModel
import java.io.{FileInputStream, File}

import ar.com.hjg.pngj.chunks.{PngChunkTextVar, ChunkCopyBehaviour}
import ar.com.hjg.pngj._
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
  //.filter(f => f.getName.startsWith("58_testpdf.pdf-")).toList

  pngFiles.foreach(pngFile => {
    try {
      logger.debug("File: " + pngFile.getName)
      val in = new PngReader(pngFile)

      logger.debug(in.imgInfo.toStringDetail)

      val coordsYellow = new mutable.HashMap[Int, Int]()
      val coordsGreen = new mutable.HashMap[Int, Int]()

      for(row <- 0 to in.imgInfo.rows) {
        val l1 = in.readRow()
        //val scanline = l1.asInstanceOf[ImageLineInt].getScanline.toList
        for(col <- 0 to in.imgInfo.cols) {
          val rgb8 = ImageLineHelper.getPixelRGB8(l1, col)

          if(getDifference(Color.yellow.getRGB, rgb8) < 300) {
            logger.debug("YELLOW match FOUND. file: " + pngFile.getName)
            logger.debug("Colors JAVA: " + Color.yellow.getRGB + ", PNGJ: " + rgb8)
            coordsYellow += (row -> col)
            val out = new PngWriter(new File("snippets/"+pngFile.getName), in.imgInfo, true)
            out.copyChunksFrom(in.getChunksList(), ChunkCopyBehaviour.COPY_ALL)
            out.getMetadata().setText(PngChunkTextVar.KEY_Description, "Identify highlighted text")
            for (roww <- 0 to in.imgInfo.rows) {
              out.writeRow(in.readRow(roww))
            }
            out.end()
            logger.debug("End image write")

          } else if(getDifference(Color.green.getRGB, rgb8) < 300) {
            logger.debug("GREEN match FOUND. file: " + pngFile.getName)
            logger.debug("Colors JAVA: " + Color.yellow.getRGB + ", PNGJ: " + rgb8)
            coordsGreen += (row -> col)
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
