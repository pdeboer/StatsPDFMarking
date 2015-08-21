import java.awt.Color
import java.io._

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightPage, HighlightTermloader, PDFHighlightInstruction, PDFPermuter}
import input.folder.FolderPDFSource
import org.codehaus.plexus.util.FileUtils
import org.joda.time.DateTime
import snippet.MainSnippet

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging {

  val pdfsDir = "../pdfs2/"
  val snippetsDir = "../merge_method_snippets/"

  val pathConvert = "/opt/local/bin/convert"

  val PERMUTATIONS_CSV_FILENAME = "permutations.csv"

  val startTime = new DateTime().getMillis

  val filterDirectories = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir,name).isDirectory
  }

  new File(snippetsDir).mkdir()

  emptySnippetsDir(new File(snippetsDir))

  highlightPDFFile

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
    val permutations : List[Option[List[Permutation]]] = new FolderPDFSource(pdfsDir).get().par.flatMap(f => {
      highlightFile(f)
    }).toList

    val writer = new PrintWriter(new File(PERMUTATIONS_CSV_FILENAME))
    writer.write("group_name, method_index, snippet_filename, pdf_path, state\n")
    permutations.foreach(p => {
      if(p.isDefined){
        p.get.foreach(pe => {
          writer.append(pe.groupName + "," + pe.methodIndex+ "," + pe.snippetPath + "," + pe.pdfPath + ", 0\n")
        })
      }
    })

    writer.close()

  }

  logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")

  case class StatMethod(minIndex:Int, maxIndex:Int, children:List[StatMethod], instructions: List[PDFHighlightInstruction])

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
        List[StatMethod](method1, method2) ::: method1.children ::: method2.children, method1.instructions ::: method2.instructions))
    }
    else {
      List[StatMethod](method1, method2)
    }
  }

  def areMergeable(method1: StatMethod, method2: StatMethod): Boolean = {
    method1.maxIndex > method2.minIndex
  }

  def highlightFile(f: File) : List[Option[List[Permutation]]] = {
    val terms = new HighlightTermloader

    val permutations: List[Option[List[Permutation]]] = terms.termNames.par.map(method => {

      val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get

      val onlyMethods: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.name) ::: methodAndSynonyms.synonyms))

      try {
        val permuter = new PDFPermuter(f.getAbsolutePath)
        val maxLengthPDF = permuter.txt.map(_.length).sum

        val methodList = permuter.findAllMethodsInPaper(onlyMethods).sortBy(m => {
          permuter.txt.zipWithIndex.filter(_._2<m.pageNr).map(_._1.length).sum +
            m.startSearchStringIndex + m.startHighlightStringIndex
        })

        val methodName = method.replaceAll(" ", "_")
        val year = f.getName.substring(0, f.getName.indexOf("_"))
        val pdfDirName = f.getName.substring(f.getName.indexOf("_") + 1, f.getName.length - 4)


        var mergedMethods = methodList.map(m => {
          StatMethod(
            Math.max(0, permuter.txt.zipWithIndex.filter(_._2<m.pageNr).map(_._1.length).sum + m.startSearchStringIndex + m.startHighlightStringIndex - 10000),
            Math.min(maxLengthPDF, permuter.txt.zipWithIndex.filter(_._2<m.pageNr).map(_._1.length).sum + m.startSearchStringIndex + m.startHighlightStringIndex + 10000),
            List.empty[StatMethod],
            List[PDFHighlightInstruction](m))
        })

        if(methodList.nonEmpty) {
          var changedSomething = false
          do {
            val tmpList = combine(mergedMethods)
            changedSomething = !(tmpList equals mergedMethods)
            mergedMethods = tmpList
          }while(changedSomething)

          if(mergedMethods.nonEmpty) {
            val assumptionsForMethod : List[String] = methodAndSynonyms.assumptions.flatMap(assumption => {
              List[String](assumption.name) ::: assumption.synonym
            })

            val assumptionsList = permuter.getUniqueStringsForSearchTerms(Map(Color.green -> assumptionsForMethod)).toList
            if(assumptionsList.nonEmpty) {
              logger.debug(s"Result after merging method: $method => ${mergedMethods.length} different groups.")
              Some(mergedMethods.par.zipWithIndex.flatMap(groupedMethods => {
                createHighlightedPDF(groupedMethods._2, groupedMethods._1.instructions, assumptionsList, method, f)
              }).toList)
            }else {
              None
            }
          }else{
            None
          }
        }else {
          None
        }
      } catch {
        case e: Exception => {
          logger.error(s"Error while highlighting permutations for file $f", e)
          new File("../errors_whilePermuting").mkdir()
          val pdf = new File("../errors_whilePermuting/"+f.getName)

          try{
            FileUtils.copyFile(f, pdf)
          }catch {
            case e: Exception => {
              logger.error(s"Cannot copy file $f to ../errors_whilePermuting/ directory!", e)
              None
            }
          }
          None
        }
      }
    }).toList

    permutations
  }

  case class Permutation(groupName: String, methodIndex: String, snippetPath: String, pdfPath: String)

  def createHighlightedPDF(groupId: Int, methodsList: List[PDFHighlightInstruction], assumptionsList: List[PDFHighlightInstruction], method: String, f: File): List[Permutation] = {

    new PDFPermuter(f.getAbsolutePath).getUniquePairsForSearchTerms(methodsList, assumptionsList).zipWithIndex.par.flatMap( highlighter => {

      logger.debug(s"${highlighter._2}_${f.getName}: highlighting combination of ${highlighter._1.instructions}")

      val methodName = method.replaceAll(" ", "_")
      val year = f.getName.substring(0, f.getName.indexOf("_"))
      val pdfDirName = f.getName.substring(f.getName.indexOf("_") + 1, f.getName.length - 4)

      val pathToSavePDFs = snippetsDir + "/" + year + "/" + methodName + "/" + pdfDirName
      new File(pathToSavePDFs).mkdirs()

      val highlightedFile = new File(pathToSavePDFs + "/" + f.getName.substring(0, f.getName.length - 4) + "_" + highlighter._2 + ".pdf")

      val highlightedPaper = highlighter._1.highlight()
      Some(new BufferedOutputStream(new FileOutputStream(highlightedFile))).foreach(s => {
        s.write(highlightedPaper._2)
        s.close()
      })

      logger.debug(s"Converting $f to PNG (pages: [${highlightedPaper._1.start},${highlightedPaper._1.end}])...")
      val pdfToPngPath = convertPDFtoPNG(highlightedFile, highlightedPaper._1)

      logger.debug("Cutting snippet...")
      val snippetPath = MainSnippet.createSnippet(new File(pdfToPngPath))

      val methodInstructions = highlighter._1.instructions.filter(f => f.color==Color.yellow)
      val methodPositions = methodInstructions.map(methodInstruction => {
        methodInstruction.pageNr + ":" + (methodInstruction.startSearchStringIndex + methodInstruction.startHighlightStringIndex)
      }).mkString("_")


      val permutations = highlighter._1.instructions.map( i => {
        if(i.color==Color.green) {
          val assumptionPosition = i.pageNr+":"+(i.startSearchStringIndex+i.startHighlightStringIndex)
          Permutation(f.getName.substring(0, f.getName.indexOf(".pdf")+4)+"/"+i.highlightString+"/"+assumptionPosition, methodName+"_"+methodPositions, snippetPath, highlightedFile.getPath)
        }
      })

      permutations.filter(p => p.isInstanceOf[Permutation]).map(_.asInstanceOf[Permutation])
    }).toList
  }

  def convertPDFtoPNG(pdfFile: File, pages: HighlightPage) : String = {
    val pathPDFFile = pdfFile.getPath
    val pathConvertedPNGFile: String = pdfFile.getParentFile.getPath+"/"+createPNGFileName(pdfFile.getName)

    val range = if(pages.start != pages.end){
      "["+pages.start+"-"+pages.end+"]"
    }else {
      "["+pages.start+"]"
    }

    val convertCommandWithParams =
      Seq("bash", "-c", s"nice -n 5 $pathConvert -density 200 -append ${pathPDFFile + range} ${pathConvertedPNGFile}")

    if(convertCommandWithParams.! != 0){
      logger.error(s"File: ${pdfFile.getName} cannot be converted to PNG")
      new File("../errors_convertPDFtoPNG").mkdir()
      val pdf = new File("../errors_convertPDFtoPNG/"+pdfFile.getName)
      try{
        FileUtils.copyFile(pdfFile, pdf)
      }catch {
        case e: Exception => logger.error(s"Cannot copy file $pdfFile to ../errors_convertToPNG/ directory!", e)
      }
      ""
    }else {
      logger.debug(s"File: ${pdfFile.getName} successfully converted to PNG")
      pathConvertedPNGFile
    }
  }

  def createPNGFileName(filename: String) : String = {
    filename+".png"
  }

  def removePDFExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
  }
}
