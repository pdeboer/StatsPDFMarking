import java.awt.Color
import java.io.{BufferedOutputStream, File, FileOutputStream, FilenameFilter}

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, PDFPermuter}
import input.folder.FolderPDFSource
import org.joda.time.DateTime

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging{

  val pdfsDir = "../pdfs/"
	val outputDir = "../output/"

  val pathConvert = "/opt/local/bin/convert"
  val pathGS = "/opt/local/bin/gs"

  val startTime = new DateTime().getMillis

	createOrEmptyOutputDir

  convertToBlackAndWhiteAndHighlightTerms

  logger.debug("Starting conversion PDF2PNG...")

  new File(outputDir).listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
  }).par.foreach(pdfFile => {
    convertPDFtoPNG(pdfFile)
  })

  logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime)/1000} seconds")


  def createOrEmptyOutputDir = {
    new File(outputDir).mkdirs()
    new File(outputDir).listFiles().foreach(f => {
      if (f.isDirectory) {
        f.listFiles().foreach(img => img.delete())
      }
      f.delete()
    })
  }

  def convertToBlackAndWhiteAndHighlightTerms = {
    new FolderPDFSource(pdfsDir).get().par.foreach(f => {
      /*
      val grayPdfName = f.getPath.substring(0, f.getPath.size-4)+"_gray.pdf"

      logger.info("Converting pdf to Black and white...")
      (pathGS + " -sOutputFile=" + grayPdfName + " -sDEVICE=pdfwrite -sColorConversionStrategy=Gray -dProcessColorModel=/DeviceGray -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH " + f.getPath).!!
      */
      highlightFile(f)
      logger.info(s"processed $f")

    })
  }

  def highlightFile(f: File) = {
    val terms = new HighlightTermloader
    val colorToStrings: Map[Color, List[String]] = Map(Color.yellow -> terms.methodsAndSynonyms, Color.green -> terms.assumptionsAndSynonyms)

    new PDFPermuter(f.getAbsolutePath).permuteForEachCombinationOf(colorToStrings).zipWithIndex.par.foreach(
      highlighter => {
        logger.debug(s"${highlighter._2}_${f.getName}: highlighting combination of ${highlighter._1.instructions}")

        Some(new BufferedOutputStream(new FileOutputStream(outputDir + highlighter._2 + "_" + f.getName))).foreach(s => {
          s.write(highlighter._1.highlight())
          s.close()
        })
      })
  }

  def convertPDFtoPNG(pdfFile: File) = {
    val pathPDFFile = pdfFile.getPath
    val pathConvertedPNGFile: String = createSubDirForPNGs(pdfFile)

    val convertCommandWithParams = pathConvert + " -density 200 "

    try {
      (convertCommandWithParams + pathPDFFile + " " + pathConvertedPNGFile).!!
      logger.debug(s"File: ${pdfFile.getName} successfully converted to PNG")
    } catch {
      case e: Exception => logger.error(s"Cannot convert ${pdfFile.getName} to PNG.",e)
    }
  }

  def createSubDirForPNGs(pdfFile: File): String = {
    val subDirPNGFiles = outputDir + removePDFExtension(pdfFile.getName)
    val pathConvertedPNGFile = subDirPNGFiles + "/" + createPNGFileName(pdfFile.getName)

    new File(subDirPNGFiles).mkdirs()
    pathConvertedPNGFile
  }

  def createPNGFileName(filename: String) : String = {
    filename+".png"
  }

  def removePDFExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
  }
}
