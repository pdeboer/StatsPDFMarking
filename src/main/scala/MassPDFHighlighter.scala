import java.awt.Color
import java.io._

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import highlighting.Alg250.StatMethod
import highlighting._
import input.folder.FolderPDFSource
import org.codehaus.plexus.util.FileUtils
import org.joda.time.DateTime
import snippet.Snippet

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging {

  val conf = ConfigFactory.load()

  val pdfsDir = conf.getString("highlighter.pdfSourceDir")
  val snippetsDir = conf.getString("highlighter.snippetDir")
  val pathConvert = conf.getString("highlighter.convertCmd")
  val PERMUTATIONS_CSV_FILENAME = conf.getString("highlighter.permutationFilename")

  val startTime = new DateTime().getMillis

  new File(snippetsDir).mkdir()

  emptySnippetsDir(new File(snippetsDir))

  highlightPDFFile

  logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")

  def emptySnippetsDir(dir: File): Boolean = {
    dir.listFiles().par.foreach(file => {
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
    writer.write("group_name, method_index, snippet_filename, pdf_path, method_on_top, relative_height_top, relative_height_bottom\n")
    permutations.foreach(p => {
      if(p.isDefined) {
        p.get.foreach(pe => {
          val methodOnTop = if(pe.methodOnTop) 1 else 0
          writer.append(pe.groupName + "," + pe.methodIndex+ "," + pe.snippetPath + "," + pe.pdfPath + "," + methodOnTop + "," +  pe.relativeTop +","+ pe.relativeBottom+"\n")
        })
      }
    })
    writer.close()
  }


  def highlightFile(f: File) : List[Option[List[Permutation]]] = {
    val terms = new HighlightTermloader

    terms.termNames.par.map(method => {

      val delta = terms.getDeltaForMethod(method)
      val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get
      val availableMethods: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.name) ::: methodAndSynonyms.synonyms))

      try {
        val permuter = new PDFPermuter(f.getAbsolutePath)
        val maxLengthPDF = permuter.txt.map(_.length).sum

        val methodList = permuter.findAllMethodsInPaper(availableMethods).sortBy(m => {
          permuter.txt.zipWithIndex.filter(_._2<m.pageNr).map(_._1.length).sum +
            m.startSearchStringIndex + m.startHighlightStringIndex
        })

        val methodsToMerge = methodList.map(m => {
          val methodIndex = calculateIndexPositionOfMethod(permuter, m)
          StatMethod(Math.max(0, methodIndex - delta), Math.min(maxLengthPDF, methodIndex + delta), List.empty[StatMethod], List[PDFHighlightInstruction](m))
        })

        mergeMethodsAndHighlightPDF(f.getName, method, methodAndSynonyms, permuter, methodsToMerge)

      } catch {
        case e: Exception => {
          copyAndMoveFile("../errors_whilePermuting", f, e)
          None
        }
      }
    }).toList
  }

  def copyAndMoveFile(dest: String, f: File, e: Exception): Any = {
    logger.error(s"Error while highlighting permutations for file $f", e)
    new File(dest).mkdir()
    val pdf = new File(dest + f.getName)

    try {
      FileUtils.copyFile(f, pdf)
    } catch {
      case e: Exception => {
        logger.error(s"Cannot copy file $f to $dest directory!", e)
        None
      }
    }
  }

  def mergeMethodsAndHighlightPDF(pdfFilename: String, method: String, methodAndSynonyms: StatisticalMethod, permuter: PDFPermuter, methodsToMerge: List[StatMethod]): Option[List[Permutation]] = {
    if (methodsToMerge.nonEmpty) {
      val (mergedMethods: List[StatMethod], assumptionsList: List[PDFHighlightInstruction]) =
        Alg250.mergeMethods(methodsToMerge, method, permuter, methodAndSynonyms, pdfFilename)

      Some(mergedMethods.par.zipWithIndex.flatMap(groupedMethods => {
        createHighlightedPDF(groupedMethods._2, groupedMethods._1.instructions, assumptionsList, method, pdfFilename, permuter)
      }).toList)
    } else {
      None
    }
  }

  def calculateIndexPositionOfMethod(permuter: PDFPermuter, m: PDFHighlightInstruction): Int = {
    permuter.txt.zipWithIndex.filter(_._2 < m.pageNr).map(_._1.length).sum + m.startSearchStringIndex + m.startHighlightStringIndex
  }

  def createHighlightedPDF(groupId: Int, methodsList: List[PDFHighlightInstruction], assumptionsList: List[PDFHighlightInstruction], method: String, pdfFilename: String, permuter: PDFPermuter): List[Permutation] = {

    permuter.getUniquePairsForSearchTerms(methodsList, assumptionsList).zipWithIndex.par.flatMap( highlighter => {

      logger.debug(s"${highlighter._2}_$pdfFilename: highlighting combination of ${highlighter._1.instructions}")

      val methodName = method.replaceAll(" ", "_")
      val year = extractPublicationYear(pdfFilename)

      val pdfDirName = removePDFExtension(pdfFilename)
      val pathToSavePDFs = snippetsDir + "/" + year + "/" + methodName + "/" + pdfDirName
      new File(pathToSavePDFs).mkdirs()
      val highlightedFilename = new File(pathToSavePDFs + "/" + pdfDirName + "_" + highlighter._2 + "_" + groupId + ".pdf")

      val highlightedPDFPaper = highlighter._1.highlight()
      Some(new BufferedOutputStream(new FileOutputStream(highlightedFilename))).foreach(s => {
        s.write(highlightedPDFPaper._2)
        s.close()
      })

      createPNGandPermutations(pdfFilename, highlighter, methodName, highlightedFilename, highlightedPDFPaper)
    }).toList
  }

  def extractPublicationYear(pdfFilename: String): Int = {
    try {
      if (pdfFilename.substring(0, 4).toInt >= 2002 && pdfFilename.substring(0, 4).toInt <= DateTime.now().getYear) {
        pdfFilename.substring(0, 4).toInt
      } else {
        2014
      }
    } catch {
      case e: Exception => 2014
    }
  }

  def createPNGandPermutations(pdfFilename: String, highlighter: (PDFHighlight, Int), methodName: String, highlightedFilename: File, highlightedPDFPaper: (HighlightPage, Array[Byte])): List[Permutation] = {
    logger.debug(s"Converting $pdfFilename to PNG (pages: [${highlightedPDFPaper._1.start},${highlightedPDFPaper._1.end}])...")

    val pdfToPngPath = convertPDFtoPNG(highlightedFilename, highlightedPDFPaper._1)
    val snippetPath = cutPNG(pdfToPngPath)
    val methodInstructions = highlighter._1.instructions.filter(f => f.color == Color.yellow)

    val methodPositions = methodInstructions.map(methodInstruction => {
      methodInstruction.pageNr + ":" + (methodInstruction.startSearchStringIndex + methodInstruction.startHighlightStringIndex)
    }).mkString("_")

    val permutations: List[Option[Permutation]] = highlighter._1.instructions.map(i => {
      if (i.color == Color.green) {
        Some(createPermutationForPrerequisite(methodName, highlightedFilename, snippetPath, methodPositions, i))
      } else {
        None
      }
    })
    permutations.filter(p => p.isDefined).map(_.get)
  }

  def createPermutationForPrerequisite(methodName: String, highlightedFilename: File, snippetPath: String, methodPositions: String, instruction: PDFHighlightInstruction): Permutation = {
    val assumptionPosition = instruction.pageNr + ":" + (instruction.startSearchStringIndex + instruction.startHighlightStringIndex)
    if (args.isDefinedAt(0) && args(0).equalsIgnoreCase("2")) {
      val matches = Snippet.extractColorCoords(new File(snippetPath))
      val height = Snippet.getHeight(new File(snippetPath))
      val methodOnTop = Snippet.isMethodOnTop(snippetPath)

      val boundaryMin = if (matches._1.nonEmpty && matches._2.nonEmpty) {
        Math.min(matches._1.minBy(_.getY).getY, matches._2.minBy(_.getY).getY) / height * 100
      } else {
        0.0
      }
      val boundaryMax = if (matches._1.nonEmpty && matches._2.nonEmpty) {
        Math.max(matches._1.maxBy(_.getY).getY, matches._2.maxBy(_.getY).getY) / height * 100
      } else {
        100.0
      }

      Permutation(snippetPath + "/" + instruction.highlightString + "/" + assumptionPosition, methodName + "_" + methodPositions,
        snippetPath, highlightedFilename.getPath, methodOnTop, boundaryMin, boundaryMax)
    } else {
      val methodOnTop = Snippet.isMethodOnTop(snippetPath)
      Permutation(snippetPath + "/" + instruction.highlightString + "/" + assumptionPosition, methodName + "_" + methodPositions,
        snippetPath, highlightedFilename.getPath, methodOnTop)
    }
  }

  def cutPNG(pdfToPngPath: File): String = {
    if (pdfToPngPath != null) {
      if (args.isDefinedAt(0) && args(0).equalsIgnoreCase("2")) {
        pdfToPngPath.getPath
      } else {
        logger.debug("Cutting snippet...")
        Snippet.createSnippet(pdfToPngPath)
      }
    } else {
      ""
    }
  }

  def convertPDFtoPNG(pdfFile: File, pages: HighlightPage) : File = {
    val pathPDFFile = pdfFile.getPath
    val pathConvertedPNGFile: String = pdfFile.getParentFile.getPath+"/"+createPNGFileName(pdfFile.getName)

    val range = extractPageRange(pages)

    val convertCommandWithParams =
      Seq("bash", "-c", s"nice -n 5 $pathConvert -density 200 -append ${pathPDFFile + range} ${pathConvertedPNGFile}")

    if(convertCommandWithParams.! != 0){
      copyAndMoveFile("../errors_convertPDFtoPNG/", pdfFile, new Exception(s"Cannot convert PDF pages $range to PNG"))
      null
    }else {
      logger.debug(s"File: ${pdfFile.getName} successfully converted to PNG")
      new File(pathConvertedPNGFile)
    }
  }

  def extractPageRange(pages: HighlightPage): String = {
    if (pages.start != pages.end) {
      "[" + pages.start + "-" + pages.end + "]"
    } else {
      "[" + pages.start + "]"
    }
  }

  def createPNGFileName(filename: String) : String = {
    filename+".png"
  }

  def removePDFExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
  }
}
