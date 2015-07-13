package snippet

import java.io.File

import ar.com.hjg.pngj.chunks.ChunkCopyBehaviour
import ar.com.hjg.pngj.{ImageLineHelper, ImageLineInt, PngWriter, PngReader}
import com.typesafe.scalalogging.LazyLogging

/**
 * Created by mattia on 13.07.15.
 */
object MainSnippet extends App with LazyLogging{

  val snippetsDir = "snippets/"

  new File(snippetsDir).mkdirs()
  new File(snippetsDir).listFiles().foreach(f => f.delete())

  val files : List[File] = new File("output").listFiles().filter(f => f.getName.startsWith("58_") && f.getName.endsWith(".png")).toList

  files.foreach(file => {
    val in = new PngReader(file)
    val out = new PngWriter(new File("output/"+file.getName), in.imgInfo, true)
    val channels = in.imgInfo.channels

    out.copyChunksFrom(in.getChunksList(), ChunkCopyBehaviour.COPY_ALL_SAFE)

    for(row <- 0 to in.imgInfo.rows-1) {
      val l1 = in.readRow()
      val scanline = l1.asInstanceOf[ImageLineInt].getScanline.toList
      for(col <- 0 to in.imgInfo.cols-1) {
        print(ImageLineHelper.getPixelRGB8(l1, col)+", ")
        //scanline.updated(col * channels, scanline(col * channels)/2)
        //scanline.updated(col*channels+1, ImageLineHelper.clampTo_0_255(scanline(col*channels +1) + 20))
      }
      out.writeRow(l1)
    }
    in.end()
    out.end()
  })
}
