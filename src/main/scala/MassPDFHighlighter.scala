import java.awt.Color
import java.io._

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, PDFPermuter}
import input.folder.FolderPDFSource
import org.codehaus.plexus.util.FileUtils
import org.joda.time.DateTime

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging {

  val pdfsDir = "../pdfs2/"
	val snippetsDir = "../snippets/"

	val pathConvert = "/opt/local/bin/convert"

	val startTime = new DateTime().getMillis

  val filterDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir,name).isDirectory
  }

  new File(snippetsDir).mkdir()

  emptySnippetsDir(new File(snippetsDir))

  highlightPDFFile

	logger.debug("Starting conversion PDF2PNG...")

  val allPdfFiles :List[File] = new File(snippetsDir).listFiles(filterDirectories).par.flatMap(yearDir => {
    yearDir.listFiles(filterDirectories).par.flatMap(methodDir => {
      methodDir.listFiles(filterDirectories).par.flatMap(pdfDir => {
        pdfDir.listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
        }).map(file => file)
      }).toList
    }).toList
  }).toList

  allPdfFiles.par.foreach(convertPDFtoPNG(_))

	logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")


  def emptySnippetsDir(dir: File): Boolean = {
    dir.listFiles().foreach(file => {
      if (file.isDirectory) {
        emptySnippetsDir(file)
      }
      file.delete()
    })
    true
  }

  def highlightPDFFile = {
    new FolderPDFSource(pdfsDir).get().par.foreach(f => {
      highlightFile(f)
      logger.info(s"processed $f")
    })
  }

  def highlightFile(f: File) = {
    val terms = new HighlightTermloader

    terms.termNames.par.foreach(method => {

      val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get
      val assumptionsAndSynonyms : List[String] = methodAndSynonyms.assumptions.flatMap(assumption => {
				List[String](assumption.name) ::: assumption.synonym
			})

			val colorToStrings: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.name) ::: methodAndSynonyms.synonyms),
				Color.green -> assumptionsAndSynonyms)
      try {
        new PDFPermuter(f.getAbsolutePath).permuteForEachCombinationOf(colorToStrings).zipWithIndex.par.foreach(
          highlighter => {
            logger.debug(s"${highlighter._2}_${f.getName}: highlighting combination of ${highlighter._1.instructions}")

            val methodName = terms.getMethodFromSynonymOrMethod(highlighter._1.instructions.head.highlightString).get.name.replaceAll(" ", "_")
            val year = f.getName.substring(0, f.getName.indexOf("_"))
            val pdfDirName = f.getName.substring(f.getName.indexOf("_") + 1, f.getName.length - 4)

            val pathToSavePDFs = snippetsDir + "/" + year + "/" + methodName + "/" + pdfDirName
            new File(pathToSavePDFs).mkdirs()

            Some(new BufferedOutputStream(new FileOutputStream(pathToSavePDFs + "/" + f.getName.substring(0, f.getName.length - 4) + "_" + highlighter._2 + ".pdf"))).foreach(s => {
              s.write(highlighter._1.highlight())
              s.close()
            })
          })

      } catch {
        case e: Exception => {
          logger.error(s"Error while higlighting permutations for file $f", e)
          new File("../errors_whilePermuting").mkdir()
          val pdf = new File("../errors_whilePermuting/"+f.getName)
          try{
            FileUtils.copyFile(f, pdf)
          }catch {
            case e: Exception => logger.error(s"Cannot copy file $f to ../errors_whilePermuting/ directory!", e)
          }
        }
      }
    })
  }

  def convertPDFtoPNG(pdfFile: File) = {
    val pathPDFFile = pdfFile.getPath
    val pathConvertedPNGFile: String = pdfFile.getParentFile.getPath+"/"+createPNGFileName(pdfFile.getName)

    val convertCommandWithParams = "nice -n 5 " + pathConvert + " -density 200 -append "

    if((convertCommandWithParams + pathPDFFile.replaceAll(" ", "\\ ") + " " + pathConvertedPNGFile.replaceAll(" ", "\\ ")).! != 0){
        logger.error(s"File: ${pdfFile.getName} cannot be converted to PNG")
        new File("../errors_convertPDFtoPNG").mkdir()
        val pdf = new File("../errors_convertPDFtoPNG/"+pdfFile.getName)
        try{
          FileUtils.copyFile(pdfFile, pdf)
        }catch {
          case e: Exception => logger.error(s"Cannot copy file $pdfFile to ../errors_convertToPNG/ directory!", e)
        }
    }else {
      logger.debug(s"File: ${pdfFile.getName} successfully converted to PNG")
    }
  }

  def createPNGFileName(filename: String) : String = {
    filename+".png"
  }

  def removePDFExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
  }
}
