import java.awt.Color
import java.io.{BufferedOutputStream, File, FileOutputStream, FilenameFilter}

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, PDFPermuter}
import input.folder.FolderPDFSource

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging{
	val outputDir = "../output/"

	new File(outputDir).mkdirs()
	new File(outputDir).listFiles().foreach(f => {
    if(f.isDirectory){
      f.listFiles().foreach(img => img.delete())
    }
    f.delete()
  })


	new FolderPDFSource("../1750paper/").get().par.foreach(f => {
		highlightFile(f)

		println(s"processed $f")
	})

	def highlightFile(f: File): Unit = {
    val terms = new HighlightTermloader
    val colorToStrings: Map[Color, List[String]] = Map(Color.yellow -> terms.methodsAndSynonyms, Color.green -> terms.assumptionsAndSynonyms)


    new PDFPermuter(f.getAbsolutePath).permuteForEachCombinationOf(colorToStrings).zipWithIndex.foreach(highlighter => {
      print(s"${highlighter._2}: highlighting combination of ${highlighter._1.instructions}")

      Some(new BufferedOutputStream(new FileOutputStream(outputDir + highlighter._2 + "_" + f.getName))).foreach(s => {
        s.write(highlighter._1.highlight())
        s.close()
      })
    })
  }

  logger.debug("Starting conversion PDF2PNG...")

  new File(outputDir).listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
  }).par.foreach(pdfFile => {

    val pathPDFFile = pdfFile.getPath
    val subDirPNGFiles = outputDir + removePDFExtension(pdfFile.getName)
    val pathConvertedPNGFile = subDirPNGFiles + "/" + createPNGFileName(pdfFile.getName)

    new File(subDirPNGFiles).mkdirs()

    val convertCommandWithParams = "/opt/local/bin/convert -density 200 "

    try {
      (convertCommandWithParams + pathPDFFile + " " + pathConvertedPNGFile).!!

      logger.debug("File: " + pdfFile.getName + ", successfully converted to PNG")
    } catch {
      case e: Exception => e.printStackTrace()
    }
  })


  def createPNGFileName(filename: String) : String = {
    filename+".png"
  }

  def removePDFExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
  }
}
