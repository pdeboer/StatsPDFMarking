package png

import java.awt.Color
import java.io.File

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightPage, PDFHighlighter}
import pdf.{PDFHighlightInstruction, Permutation}
import snippet.Snippet
import utils.Utils

import scala.sys.process._

/**
 * Created by mattia on 02.09.15.
 */
case class PNGManager(isMultipleColumnPaper: Boolean, pathConvert: String) extends LazyLogging{

  def convertPDFAndCreatePermutations(highlighter: PDFHighlighter, methodName: String, highlightedFilename: File,
                               highlightedPDFPaper: HighlightPage): List[Permutation] = {
    
    val pdfToPngPath = convertToPNG(highlightedFilename, highlightedPDFPaper)
    val snippetPath = cutPNG(pdfToPngPath)
    val methodInstructions = highlighter.instructions.filter(f => f.color == Color.yellow)

    val methodPositions = methodInstructions.map(methodInstruction => {
      methodInstruction.pageNr + ":" + (methodInstruction.startSearchStringIndex + methodInstruction.startHighlightStringIndex)
    }).mkString("_")

    val permutations: List[Option[Permutation]] = highlighter.instructions.map(i => {
      if (i.color == Color.green) {
        Some(createPermutationForPrerequisite(methodName, highlightedFilename, snippetPath, methodPositions, i))
      } else {
        None
      }
    })
    permutations.filter(p => p.isDefined).map(_.get)
  }

  private def convertToPNG(pdfFile: File, pages: HighlightPage) : File = {
    val pathPDFFile = pdfFile.getPath
    val pathConvertedPNGFile: String = pdfFile.getParentFile.getPath+"/" + addPNGExtension(pdfFile.getName)

    val range = extractPageRange(pages)

    val convertCommandWithParams =
      Seq("bash", "-c", s"nice -n 5 $pathConvert -density 200 -append ${pathPDFFile + range} ${pathConvertedPNGFile}")

    if(convertCommandWithParams.! != 0){
      Utils.copyAndMoveFile("../errors_convertPDFtoPNG/", pdfFile, new Exception(s"Cannot convert PDF pages $range to PNG"))
      null
    }else {
      logger.debug(s"File: ${pdfFile.getName} successfully converted to PNG")
      new File(pathConvertedPNGFile)
    }
  }

  private def cutPNG(pdfToPngPath: File): String = {
    if (pdfToPngPath != null) {
      if (isMultipleColumnPaper) {
        pdfToPngPath.getPath
      } else {
        logger.debug("Cutting snippet...")
        Snippet.createSnippet(pdfToPngPath)
      }
    } else {
      logger.error("Cannot cut PNG file. The PDF was not correctly converted.")
      ""
    }
  }

  def createPermutationForPrerequisite(methodName: String, highlightedFilename: File, snippetPath: String,
                                       methodPositions: String, instruction: PDFHighlightInstruction): Permutation = {

    val assumptionPosition = instruction.pageNr + ":" + (instruction.startSearchStringIndex + instruction.startHighlightStringIndex)
    val pdfDirName = highlightedFilename.getParentFile.getName

    if (isMultipleColumnPaper) {
      val matches = Snippet.extractColorCoords(new File(snippetPath))
      val height = Snippet.getHeight(new File(snippetPath))

      val methodOnTop = Snippet.isMethodOnTop(snippetPath)

      val coordinatesGreen : Double = if(matches._2.nonEmpty) {
        matches._2.map(_.getY).min/height
      }else {
        0.0
      }

      val closestYellow : Double = if(matches._1.nonEmpty){
        matches._1.minBy(v => math.abs((v.getY/height) - coordinatesGreen)).getY/height
      }else{
        0.0
      }

      val boundaryMin = Math.min(coordinatesGreen, closestYellow) * 100
      val boundaryMax = Math.max(coordinatesGreen, closestYellow) * 100

      Permutation(pdfDirName + "/" + instruction.highlightString + "/" + assumptionPosition, methodName + "_" + methodPositions,
        snippetPath, highlightedFilename.getPath, methodOnTop, boundaryMin, boundaryMax)
    } else {
      val methodOnTop = Snippet.isMethodOnTop(snippetPath)
      Permutation(pdfDirName + "/" + instruction.highlightString + "/" + assumptionPosition, methodName + "_" + methodPositions,
        snippetPath, highlightedFilename.getPath, methodOnTop)
    }
  }

  def extractPageRange(pages: HighlightPage): String = {
    if (pages.start != pages.end) {
      "[" + pages.start + "-" + pages.end + "]"
    } else {
      "[" + pages.start + "]"
    }
  }

  def addPNGExtension(filename: String) : String = {
    filename+".png"
  }
}
