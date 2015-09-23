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
class PNGManager(isMultipleColumnPaper: Boolean, pathConvert: String) extends LazyLogging{

  def convertPDFAndCreatePermutations(highlighter: PDFHighlighter, methodName: String, highlightedFilename: File,
                               highlightedPDFPaper: HighlightPage): List[Permutation] = {
    
    val pdfToPngFile = convertToPNG(highlightedFilename, highlightedPDFPaper)
    val snippetPath = cutPNG(pdfToPngFile)
    val methodInstructions = highlighter.instructions.filter(_.color == Color.yellow)

    val methodPositions = methodInstructions.map(methodInstruction => {
      methodInstruction.pageNr + ":" + (methodInstruction.startSearchStringIndex + methodInstruction.startHighlightStringIndex)
    }).mkString("_")

    val permutations: List[Permutation] = highlighter.instructions.filter(_.color == Color.green).map(i => {
      createPermutationForSinglePrerequisite(PNGInstruction(methodName, highlightedFilename, snippetPath, methodPositions), i)
    })
    permutations
  }

  private def convertToPNG(pdfFile: File, pages: HighlightPage) : File = {

    val pathConvertedPNGFile: String = pdfFile.getParentFile.getPath+"/" + addPNGExtension(pdfFile.getName)

    val range = extractPageRange(pages)

    val convertCommandWithParams =
      Seq("bash", "-c", s"nice -n 5 $pathConvert -density 200 -append ${pdfFile.getPath + range} ${pathConvertedPNGFile}")

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
  
  case class PNGInstruction(methodName: String, highlightedFilename: File, snippetPath: String, methodPositions: String)
  
  def createPermutationForSinglePrerequisite(pngInstruction: PNGInstruction, instruction: PDFHighlightInstruction): Permutation = {

    val prerequisitePosition = instruction.pageNr + ":" + (instruction.startSearchStringIndex + instruction.startHighlightStringIndex)
    val pdfDirName = pngInstruction.highlightedFilename.getParentFile.getName

    if (isMultipleColumnPaper) {
      val matches = Snippet.extractColorCoords(new File(pngInstruction.snippetPath))
      val height = Snippet.getHeight(new File(pngInstruction.snippetPath))

      val methodOnTop = Snippet.isMethodOnTop(pngInstruction.snippetPath)

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

      Permutation(pdfDirName + "/" + instruction.highlightString + "/" + prerequisitePosition, pngInstruction.methodName + "_" + pngInstruction.methodPositions,
        pngInstruction.snippetPath, pngInstruction.highlightedFilename.getPath, methodOnTop, boundaryMin, boundaryMax)
    } else {
      val methodOnTop = Snippet.isMethodOnTop(pngInstruction.snippetPath)
      Permutation(pdfDirName + "/" + instruction.highlightString + "/" + prerequisitePosition, pngInstruction.methodName + "_" + pngInstruction.methodPositions,
        pngInstruction.snippetPath, pngInstruction.highlightedFilename.getPath, methodOnTop)
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
