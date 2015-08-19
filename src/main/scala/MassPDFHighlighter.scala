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
	val snippetsDir = "../merge_method_snippets/"

	val pathConvert = "/opt/local/bin/convert"

	val startTime = new DateTime().getMillis

  val filterDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir,name).isDirectory
  }

  new File(snippetsDir).mkdir()

  emptySnippetsDir(new File(snippetsDir))

  highlightPDFFile

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

  case class StatMethod(minIndex:Int, maxIndex:Int, children:List[StatMethod], superMethodenIndex: PDFHighlightInstruction)

  def combine(myList: List[StatMethod]) : List[StatMethod] = {
    val zipped1 = myList.zipWithIndex.filter(m => m._2 % 2 == 0)
    val zipped2 = myList.zipWithIndex.filter(m => m._2 % 2 == 1)

    val newList : List[StatMethod] =
      zipped1 zip zipped2 flatMap {
          case (left, right) => mergeIfMergeable(left._1, right._1)
      }


    if(newList.length>=2){
      newList.splitAt(newList.length-2)._1 ::: mergeIfMergeable(newList(newList.length-2), newList.last)
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

    terms.termNames.par.foreach(method => {

      val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get

      val onlyMethods: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.name) ::: methodAndSynonyms.synonyms))

      try {
        val permuter = new PDFPermuter(f.getAbsolutePath)
        //TODO: calculate text length
        val maxLengthPDF=permuter.txt.length
        //TODO: add page padding
        val methodList = permuter.findAllMethodsInPaper(onlyMethods).sortBy(m => m.startHighlightStringIndex+m.startHighlightStringIndex)

        val methodName = method.replaceAll(" ", "_")
        val year = f.getName.substring(0, f.getName.indexOf("_"))
        val pdfDirName = f.getName.substring(f.getName.indexOf("_") + 1, f.getName.length - 4)


        var methodList2 = methodList.map(m => {
          StatMethod(
            Math.max(0, m.startSearchStringIndex + m.startHighlightStringIndex - 10000),
            Math.min(maxLengthPDF, m.startSearchStringIndex + m.startHighlightStringIndex + 10000),
            List.empty[StatMethod],
            m)
        })

        if(methodList.nonEmpty) {
          var changedSomething = false
          do {
            val tmpList = combine(methodList2)
            changedSomething = !(tmpList equals methodList2)
            methodList2 = tmpList
          }while(changedSomething)

          if(methodList2.nonEmpty) {
            val assumptionsAndSynonyms : List[String] = methodAndSynonyms.assumptions.flatMap(assumption => {
              List[String](assumption.name) ::: assumption.synonym
            })

            val assumptionList = permuter.getUniqueStringsForSearchTerms(Map(Color.green -> assumptionsAndSynonyms))
            if(assumptionList.nonEmpty) {
              val methodsList: List[PDFHighlightInstruction] = methodList2.map(_.superMethodenIndex)
              val permutations: List[PDFHighlightInstruction] = List.concat(methodsList,assumptionList)

              createHighlightedPDF(permutations, method, f)
            }
          }
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
    new PDFPermuter(f.getAbsolutePath).getUniquePairsForSearchTerms(methodList).zipWithIndex.par.foreach(highlighter => {

      logger.debug(s"${highlighter._2}_${f.getName}: highlighting combination of ${highlighter._1.instructions}")

      val methodName = method.replaceAll(" ", "_")
      val year = f.getName.substring(0, f.getName.indexOf("_"))
      val pdfDirName = f.getName.substring(f.getName.indexOf("_") + 1, f.getName.length - 4)

      val pathToSavePDFs = snippetsDir + "/" + year + "/" + methodName + "/" + pdfDirName
      new File(pathToSavePDFs).mkdirs()

      Some(new BufferedOutputStream(new FileOutputStream(pathToSavePDFs + "/" + f.getName.substring(0, f.getName.length - 4) + "_" + highlighter._2 +".pdf"))).foreach(s => {
        s.write(highlighter._1.highlight())
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
