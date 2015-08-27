package highlighting

import java.awt.Color
import java.io.{ByteArrayOutputStream, FileInputStream}
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument

import scala.collection.immutable.Iterable

case class PDFHighlightInstruction(color: Color, searchString: String, highlightString: String, startSearchStringIndex: Int, startHighlightStringIndex: Int, pageNr: Int)

object PDFTextExtractor extends LazyLogging{
  def extract(pdfPath: String): List[String] = {
    try {
      val parser: PDFParser = new PDFParser(new FileInputStream(pdfPath))
      parser.parse()
      val pdDoc: PDDocument = new PDDocument(parser.getDocument)

      val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
      pdfHighlight.setLineSeparator(" ")
      pdfHighlight.initialize(pdDoc)

      val txt: List[String] = (0 to pdDoc.getNumberOfPages).map(pdfHighlight.textCache.getText(_)).toList
      pdDoc.close()

      txt
    } catch {
      case e: Exception => {
        logger.error(s"Cannot decode text for pdf $pdfPath", e)
        throw e
      }
      case e1: Error => {
        logger.error("An error occurred while extracting text from pdf ", e1)
        throw e1
      }
    }
	}
}

class PDFPermuter(pdfPath: String) extends LazyLogging {

  val ALLOWED_MAX_LENGTH_IN_WORD_MATCH = 7

	lazy val txt = PDFTextExtractor.extract(pdfPath)

	def findAllMethodsInPaper(permutationDefinition: Map[Color, List[String]]): List[PDFHighlightInstruction] = {
		val uniqueStrings = getUniqueStringsForSearchTerms(permutationDefinition)
    uniqueStrings.toList
	}

  def getUniquePairsForSearchTerms(methodsList: List[PDFHighlightInstruction], assumptionsList: List[PDFHighlightInstruction]): List[PDFHighlight] = {
    assumptionsList.map(p => new PDFHighlight(pdfPath, methodsList.toList ::: List(p)))
  }

  def cleanUniquePairsCandidate(seqUniqueStrings: Seq[PDFHighlightInstruction], methodIndex: Int, assumptionIndex: Int): Option[(PDFHighlightInstruction, PDFHighlightInstruction)] = {
    
    if (isHighlightAssumptionOutsideMethod(seqUniqueStrings, methodIndex, assumptionIndex)) {
      Some(seqUniqueStrings(methodIndex), seqUniqueStrings(assumptionIndex))
    }else {
      None
    }
  }

  def isHighlightAssumptionOutsideMethod(seqUniqueStrings: Seq[PDFHighlightInstruction], methodIndex: Int, assumptionIndex: Int): Boolean = {
    val startSearchIndexMethod = seqUniqueStrings(methodIndex).startSearchStringIndex
    val endSearchIndexMethod = startSearchIndexMethod + seqUniqueStrings(methodIndex).searchString.length

    val startHighlightIndexMethod = startSearchIndexMethod + seqUniqueStrings(methodIndex).startHighlightStringIndex
    val endHighlightIndexMethod = endSearchIndexMethod + seqUniqueStrings(methodIndex).highlightString.length

    val startSearchIndexAssumption = seqUniqueStrings(assumptionIndex).startSearchStringIndex
    val endSearchIndexAssumption = startSearchIndexAssumption + seqUniqueStrings(assumptionIndex).searchString.length

    val startHighlightIndexAssumption = startSearchIndexAssumption + seqUniqueStrings(assumptionIndex).startHighlightStringIndex
    val endHighlightIndexAssumption = startHighlightIndexAssumption + seqUniqueStrings(assumptionIndex).highlightString.length

    if (startSearchIndexMethod <= startSearchIndexAssumption && endSearchIndexMethod <= startSearchIndexAssumption && endSearchIndexAssumption >= endSearchIndexMethod) {
      true
    }
    else if (startHighlightIndexMethod <= startHighlightIndexAssumption && endHighlightIndexMethod <= startHighlightIndexAssumption && endHighlightIndexAssumption >= endHighlightIndexMethod) {
      true
    }
    else if (startSearchIndexAssumption <= startSearchIndexMethod && endSearchIndexAssumption <= startSearchIndexMethod && endSearchIndexAssumption <= endSearchIndexMethod) {
      true
    } else if (startHighlightIndexAssumption <= startHighlightIndexMethod && endHighlightIndexAssumption <= startHighlightIndexMethod && endHighlightIndexAssumption <= endHighlightIndexMethod) {
      true
    }
    else {
      false
    }
  }

  def isUniquePairValidCandidate(method: PDFHighlightInstruction, assumption: PDFHighlightInstruction): Boolean = {
    val terms = new HighlightTermloader

    method != assumption &&
    !method.highlightString.equals(assumption.highlightString) &&
      !method.searchString.equals(assumption.searchString) &&
      terms.methodsAndSynonyms.exists(m => method.highlightString.contains(m)) &&
      terms.assumptionsAndSynonyms.exists(a => assumption.highlightString.contains(a) &&
        method.color != assumption.color)
  }

  def escapeSearchString(searchString: String): String = {
    val search = searchString.replaceAll(" ", "").map(m => "\\Q"+m+"\\E"+"[\\-\\n\\r]{0,5}\\s*").mkString("")
    if(searchString.length <= ALLOWED_MAX_LENGTH_IN_WORD_MATCH || searchString.contains(" ")){
      "(?i)(\\b"+search+"\\b)"
    } else {
      "(?i)("+search+")"
    }
  }

  def getUniqueStringsForSearchTerms(highlightTerms: Map[Color, List[String]]): Iterable[PDFHighlightInstruction] = {
		highlightTerms.flatMap {
			case (color, patterns) => patterns.map(pattern => {

        txt.zipWithIndex.flatMap(pageTxt => {

          val allIndicesOfThesePatterns : Iterator[Int] = escapeSearchString(pattern).r.findAllMatchIn(pageTxt._1).map(_.start)

          // Special case: check if there is no MULTIVARIATE before ANOVA or ANALYSIS OF VARIANCE
          val indexesToDiscard: List[Int] = if(pattern.equalsIgnoreCase("ANOVA") || pattern.equalsIgnoreCase("analysis of variance")){
            val indeces = escapeSearchString("multivariate").r.findAllMatchIn(pageTxt._1).map(_.start)
            val discard = indeces.zip(allIndicesOfThesePatterns).exists(m => Math.abs(m._1 - m._2) < 20)
            if(discard){
              val iii = indeces.zip(allIndicesOfThesePatterns).filter(m => Math.abs(m._1 - m._2) < 20).toList
              iii.map(i => i._2)
            }else {
              List.empty[Int]
            }
          }else {
            List.empty[Int]
          }

          val substringIndices: Iterator[(Int, Int)] = allIndicesOfThesePatterns.filter(!indexesToDiscard.contains(_)).map(index => {
            extractSmallestBoundaryForSingleMatch(pattern, index, pageTxt._1)
          })

          val substrings = substringIndices.map(i => pageTxt._1.substring(i._1, i._2))
          substrings.map(substring => {

            //TODO: What if the searchStringMatch contains two times the word to highlight? which one is to highlight?
            try {
              val searchStringMatch = ("\\Q"+substring+"\\E").r.findFirstMatchIn(pageTxt._1).get
              val start = if (escapeSearchString(pattern).r.findFirstMatchIn(searchStringMatch.matched).isDefined) {
                escapeSearchString(pattern).r.findFirstMatchIn(searchStringMatch.matched).get.start
              } else {
                0
              }
              PDFHighlightInstruction(color, substring, pattern, searchStringMatch.start, start, pageTxt._2)
            }catch {
              case e: Exception => {
                logger.error("Cannot find term " + pattern + " in pdf "+ pdfPath,e)
                null
              }
            }
          })
        })

      })
		}.flatten
	}

  def isSmallestMatch(it: Int, indexPosition: Int, inputStringLength: Int, pageTxt: String): Int = {

    val subTxt = pageTxt.substring(Math.max(0, indexPosition - it), Math.min(pageTxt.length, indexPosition + inputStringLength + it))
    if(("(?i)(\\Q"+subTxt+"\\E)").r.findAllMatchIn(txt.mkString("")).length == 1){
        it
    }else {
      isSmallestMatch(it+1, indexPosition, inputStringLength, pageTxt)
    }
  }

  def extractSmallestBoundaryForSingleMatch(inputString: String, indexPosition: Int, pageTxt: String): (Int, Int) = {
    try{
      val it = isSmallestMatch(0, indexPosition, inputString.length, pageTxt)
      val selectedTxt = pageTxt.substring(Math.max(0, indexPosition - it), Math.min(pageTxt.length, indexPosition + inputString.length + it))
      if(selectedTxt.contains(" ") || selectedTxt.contains("-")){

        val doubleSpaces = if(selectedTxt.contains("  ")){
          selectedTxt.split("  ").length-1
        } else {0}

        val singleSpaces = if(selectedTxt.contains(" ")){
          selectedTxt.split(" ").length-1
        } else {0}

        val dashes = if(selectedTxt.contains("-")){
          selectedTxt.split("-").length-1
        } else {0}

        (Math.max(0, indexPosition - it), Math.min(pageTxt.length, indexPosition + inputString.length + doubleSpaces + singleSpaces + dashes + it))
      }else {
        (Math.max(0, indexPosition - it), Math.min(pageTxt.length, indexPosition + inputString.length + it))
      }
    }catch{
      case e: Exception => {
        e.printStackTrace()
        (0, pageTxt.length)
      }
      case e1: Error => {
        e1.printStackTrace()
        (0, pageTxt.length)
      }
    }

  }
}


/**
 * Created by pdeboer on 16/06/15.
 */
class PDFHighlight(val pdfPath: String, val instructions: List[PDFHighlightInstruction]) extends LazyLogging {


  /**
   * taken from Mattia's code and adapted
   */
  def highlight(): (HighlightPage, Array[Byte]) = {
    try {
      val file = pdfPath
      val parser: PDFParser = new PDFParser(new FileInputStream(file))
      parser.parse()
      val pdDoc: PDDocument = new PDDocument(parser.getDocument)

      val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
      pdfHighlight.setLineSeparator(" ")
      pdfHighlight.initialize(pdDoc)

      val pages : List[Int] = instructions.map(i => {

        val patterns = List(i.searchString, i.highlightString).zipWithIndex.map(s => if(s._2%2==0){Pattern.compile("\\Q"+s._1+"\\E")}else {Pattern.compile(escapeSearchString(s._1))})
        pdfHighlight.highlight(patterns.head, patterns(1), i.color, i.pageNr)
        i.pageNr-1
      })

      val byteArrayOutputStream = new ByteArrayOutputStream()

			if (pdDoc != null) {
				pdDoc.save(byteArrayOutputStream)
				pdDoc.close()
			}
			if (parser.getDocument != null) {
				parser.getDocument.close()
			}

      (HighlightPage(pages.min, pages.max), byteArrayOutputStream.toByteArray())
    } catch {
      case e: Exception => {
        logger.error(s"Cannot store highlighted version of pdf: $pdfPath.", e)
        (HighlightPage(-1, -1), Array.empty[Byte])
      }
    }
  }

  def escapeSearchString(searchString: String): String = {
    val search = searchString.replaceAll(" ", "").map(m => "\\Q" + m + "\\E" + "[\\-\\n\\r]{0,5}\\s*").mkString("")
    if(searchString.length <= 5 || searchString.contains(" ")){
      "(?i)(\\b"+search+"\\b)"
    } else {
      "(?i)("+search+")"
    }
  }
}