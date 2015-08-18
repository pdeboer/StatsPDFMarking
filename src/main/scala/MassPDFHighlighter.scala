import java.awt.Color
import java.io._

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, PDFHighlightInstruction, PDFPermuter}
import input.folder.FolderPDFSource
import org.codehaus.plexus.util.FileUtils
import org.joda.time.DateTime

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging {

  val pdfsDir = "../pdfs/"
  val snippetsDir = "../delta_snippets/"

  val pathConvert = "/usr/bin/convert"

  val startTime = new DateTime().getMillis

  val filterDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir,name).isDirectory
  }

  new File(snippetsDir).mkdir()

  emptySnippetsDir(new File(snippetsDir))

  highlightPDFFile

  logger.debug("Starting conversion PDF2PNG...")

  /*val allPdfFiles :List[File] = new File(snippetsDir).listFiles(filterDirectories).par.flatMap(yearDir => {
    yearDir.listFiles(filterDirectories).par.flatMap(methodDir => {
      methodDir.listFiles(filterDirectories).par.flatMap(pdfDir => {
        pdfDir.listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
        }).map(file => file)
      }).toList
    }).toList
  }).toList

  allPdfFiles.par.foreach(convertPDFtoPNG(_))*/

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
    new FolderPDFSource(pdfsDir).get().foreach(f => {
      highlightFile(f)
      logger.info(s"processed $f")
    })
  }

  case class StatMethod(minIndex:Int, maxIndex:Int, children:List[StatMethod], superMethodenIndex: PDFHighlightInstruction)

  def combine(myList: List[StatMethod]) : List[StatMethod] = {
    val zipped1 = myList.zipWithIndex.filter(m => m._2 % 2 == 0)
    val zipped2 = myList.zipWithIndex.filter(m => m._2 % 2 == 1)

    val newList : List[StatMethod] =
      zipped1 zip zipped2 map {
          case (left, right) => {
            mergeIfMergeable(left._1, right._1)
          }
      } flatten


    if(newList.length>=2){
      newList.splitAt(newList.length-2)._1 ::: mergeIfMergeable(newList(newList.length-2), newList(newList.length-1))
    }else {
      newList
    }
  }

  def mergeIfMergeable(method1: StatMethod, method2: StatMethod) : List[StatMethod] = {
    if(areMergeable(method1,method2)) {
      List[StatMethod](StatMethod(
        Math.min(method1.minIndex, method2.minIndex),
        Math.max(method1.maxIndex, method2.maxIndex),
        List[StatMethod](method1, method2) ::: method1.children ::: method2.children, method1.superMethodenIndex))
    }
    else {
      List[StatMethod](method1, method2)
    }
  }

  def areMergeable(method1: StatMethod, method2: StatMethod): Boolean = {
    method1.maxIndex > method2.minIndex
  }


  def highlightFile(f: File) = {
    val terms = new HighlightTermloader

    terms.termNames.foreach(method => {

      val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get

      val colorToStrings: Map[Color, List[String]] = Map(Color.RED -> (List[String](methodAndSynonyms.name) ::: methodAndSynonyms.synonyms))
      try {
        val permuter = new PDFPermuter(f.getAbsolutePath)
        val maxLengthPDF=permuter.txt.length

        val methodList = permuter.findAllMethodsInPaper(colorToStrings).sortBy(method  => method.startSearchStringIndex+method.startHighlightStringIndex)

        if(methodList.length>1){
          createHighlightedPDF(methodList, method, f)
        }

      } catch {
        case e: Exception => {
          logger.error(s"Error while highlighting permutations for file $f", e)
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

  def createHighlightedPDF(methodList: List[PDFHighlightInstruction], method: String, f: File) = {
    new PDFPermuter(f.getAbsolutePath).getUniquePairsForSearchTerms(methodList).zipWithIndex.foreach(highlighter => {

      logger.debug(s"Highlighting: ${highlighter._1._2}")

      val methodName = method.replaceAll(" ", "_")
      val year = f.getName.substring(0, f.getName.indexOf("_"))
      val pdfDirName = f.getName.substring(f.getName.indexOf("_") + 1, f.getName.length - 4)

      val pathToSavePDFs = snippetsDir + "/" + year + "/" + methodName + "/" + pdfDirName
      new File(pathToSavePDFs).mkdirs()

      Some(new BufferedOutputStream(new FileOutputStream(pathToSavePDFs + "/" + highlighter._1._1 + "-D-" + f.getName.substring(0, f.getName.length - 4) + "_" + highlighter._2 +".pdf"))).foreach(s => {
        s.write(highlighter._1._2.highlight())
        s.close()
      })
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
