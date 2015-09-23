package pdf

import java.awt.Color
import java.io._

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, MergeMethods, StatMethod, StatisticalMethod}
import input.folder.FolderPDFSource
import org.joda.time.DateTime
import png.PNGManager
import utils.Utils

case class MethodInPaper(pdfFilename: String, method: String, methodAndSynonyms: StatisticalMethod,
                permuter: PDFPermuter, methodsToMerge: List[StatMethod])

class PDFManager(isMultipleColumnPaper: Boolean, pdfsDir: String, snippetsDir: String, pathConvert: String) extends LazyLogging {

  def highlightFiles() = {
    val permutations : List[Option[List[Permutation]]] = new FolderPDFSource(pdfsDir).get().par.flatMap(f => {
      highlightFile(f)
    }).toList

    Utils.createCSV(permutations)
  }

  def highlightFile(pdfFile: File) : List[Option[List[Permutation]]] = {
    val terms = new HighlightTermloader

    terms.methods.par.map(method => {

      val delta = terms.getDeltaForMethod(method)
      val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get
      val availableMethods: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.methodName) ::: methodAndSynonyms.synonyms))

      try {
        val permuter = new PDFPermuter(pdfFile.getAbsolutePath)
        val maxLengthPDF = PDFTextExtractor.extract(pdfFile.getAbsolutePath).map(_.length).sum

				val methodList = permuter.findAllMethodsInPaper(availableMethods).sortBy(m => calculateIndexPositionOfMethod(permuter, m))

        val methodsToMerge = createStatMethodList(PaperHighlightManager(delta, permuter, methodList, maxLengthPDF))

        mergeMethodsAndHighlightPDF(MethodInPaper(pdfFile.getName, method, methodAndSynonyms, permuter, methodsToMerge))

      } catch {
        case e: Exception => {
          Utils.copyAndMoveFile("../errors_whilePermuting", pdfFile, e)
          None
        }
      }
    }).toList
  }

  case class PaperHighlightManager(delta: Int, permuter: PDFPermuter, methodList: List[PDFHighlightInstruction], maxLengthPDF: Int)

  def createStatMethodList(paperHighlightManager: PaperHighlightManager) = {
    paperHighlightManager.methodList.map(m => {
      val methodIndex = calculateIndexPositionOfMethod(paperHighlightManager.permuter, m)
      StatMethod(Math.max(0, methodIndex - paperHighlightManager.delta), Math.min(paperHighlightManager.maxLengthPDF, methodIndex + paperHighlightManager.delta), List.empty[StatMethod], List[PDFHighlightInstruction](m))
    })
  }

  def calculateIndexPositionOfMethod(permuter: PDFPermuter, m: PDFHighlightInstruction): Int = {
    permuter.txt.zipWithIndex.filter(_._2 < m.pageNr).map(_._1.length).sum + m.startSearchStringIndex + m.startHighlightStringIndex
  }

  case class HighlightInstruction(groupId: Int, methodsList: List[PDFHighlightInstruction], assumptionsList: List[PDFHighlightInstruction])

  def mergeMethodsAndHighlightPDF(methodInPaper: MethodInPaper): Option[List[Permutation]] = {
    if (methodInPaper.methodsToMerge.nonEmpty) {
      val (mergedMethods: List[StatMethod], assumptionsList: List[PDFHighlightInstruction]) = MergeMethods.mergeMethods(methodInPaper)
      Some(mergedMethods.par.zipWithIndex.flatMap(groupedMethods => {
        highlightPDFAndCreatePNG(HighlightInstruction(groupedMethods._2, groupedMethods._1.instructions, assumptionsList), methodInPaper)
      }).toList)
    } else {
      None
    }
  }
  
  def highlightPDFAndCreatePNG(toHighlight: HighlightInstruction, methodInPaper: MethodInPaper): List[Permutation] = {

    methodInPaper.permuter.getUniquePairsForSearchTerms(toHighlight.methodsList, toHighlight.assumptionsList).zipWithIndex.par.flatMap( highlighter => {

      logger.debug(s"${highlighter._2}_${methodInPaper.pdfFilename}: highlighting combination of ${highlighter._1.instructions}")
      val methodName = methodInPaper.method.replaceAll(" ", "_")
      val year = extractPublicationYear(methodInPaper.pdfFilename)

      val pdfDirName = removeExtension(methodInPaper.pdfFilename)
      val pathToSaveHighlightedPDFs = snippetsDir + "/" + year + "/" + methodName + "/" + pdfDirName
      new File(pathToSaveHighlightedPDFs).mkdirs()
      val highlightedFilename = new File(createHighlightedPDFFilename(toHighlight.groupId, highlighter._2, pdfDirName, pathToSaveHighlightedPDFs))

      val highlightedPDF = highlighter._1.highlight()
      Some(new BufferedOutputStream(new FileOutputStream(highlightedFilename))).foreach(s => {
        s.write(highlightedPDF._2)
        s.close()
      })

      logger.debug(s"Converting ${methodInPaper.pdfFilename} to PNG (pages: [${highlightedPDF._1.start},${highlightedPDF._1.end}])...")
      new PNGManager(isMultipleColumnPaper, pathConvert).convertPDFAndCreatePermutations(highlighter._1, methodName, highlightedFilename, highlightedPDF._1)
    }).toList
  }

  def removeExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
  }

  def createHighlightedPDFFilename(groupId: Int, permutationNr: Int, pdfDirName: String, pathToSaveHighlightedPDFs: String): String = {
    pathToSaveHighlightedPDFs + "/" + pdfDirName + "_" + permutationNr + "_" + groupId + ".pdf"
  }

  def extractPublicationYear(pdfFilename: String): Int = {
    try {
      if (pdfFilename.substring(0, 4).toInt >= 2009 && pdfFilename.substring(0, 4).toInt <= DateTime.now().getYear) {
        pdfFilename.substring(0, 4).toInt
      } else {
        2014
      }
    } catch {
      case e: Exception => 2014
    }
  }

}