package highlighting

import java.awt.Color
import java.io.{ByteArrayOutputStream, FileInputStream}
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument

import scala.collection.immutable.Iterable

case class PDFHighlightInstruction(color: Color, searchString: String, highlightString: String, startSearchStringIndex: Int, startHighlightStringIndex: Int)

object PDFTextExtractor extends LazyLogging{
  def extract(pdfPath: String): String = {
    try {
      val parser: PDFParser = new PDFParser(new FileInputStream(pdfPath))
      parser.parse()
      val pdDoc: PDDocument = new PDDocument(parser.getDocument)

      val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
      pdfHighlight.setLineSeparator(" ")
      pdfHighlight.initialize(pdDoc)

      val txt = (0 to pdDoc.getNumberOfPages).map(pdfHighlight.textCache.getText(_)).mkString("")
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

  val ALLOWED_MAX_LENGTH_IN_WORD_MATCH = 5

	lazy val txt = PDFTextExtractor.extract(pdfPath)

	def permuteForEachCombinationOf(permutationDefinition: Map[Color, List[String]]): Iterable[PDFHighlight] = {

		val uniqueStrings = getUniqueStringsForSearchTerms(permutationDefinition)
		val uniquePairs = getUniquePairsForSearchTerms(uniqueStrings)

		uniquePairs.map(p => new PDFHighlight(pdfPath, List(p._1, p._2)))
	}

  def getUniquePairsForSearchTerms(uniqueStrings: Iterable[PDFHighlightInstruction]): Iterable[(PDFHighlightInstruction,PDFHighlightInstruction)] = {
    val uniquePairs : IndexedSeq[Option[(PDFHighlightInstruction,PDFHighlightInstruction)]] =
      for (i <- 0 until uniqueStrings.toSeq.length; j <- i until uniqueStrings.toSeq.length; if(isUniquePairValidCandidate(uniqueStrings.toSeq(i), uniqueStrings.toSeq(j)))) yield {
        cleanUniquePairsCandidate(uniqueStrings.toSeq, i, j)
      }

    uniquePairs.filter(f=> f.isDefined).map(m => m.get).toList
  }

  def cleanUniquePairsCandidate(seqUniqueStrings: Seq[PDFHighlightInstruction], methodIndex: Int, assumptionIndex: Int): Option[(PDFHighlightInstruction, PDFHighlightInstruction)] = {
    
    if (isHighlightAssumptionOutsideMethod(seqUniqueStrings, methodIndex, assumptionIndex)) {
      Some(seqUniqueStrings(methodIndex), seqUniqueStrings(assumptionIndex))
    }else {
      None
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

  def isHighlightAssumptionOutsideMethod(seqUniqueStrings: Seq[PDFHighlightInstruction],  methodIndex: Int, assumptionIndex: Int): Boolean = {
    val startSearchIndexMethod = seqUniqueStrings(methodIndex).startSearchStringIndex
    val endSearchIndexMethod = startSearchIndexMethod + seqUniqueStrings(methodIndex).searchString.length

    val startHighlightIndexMethod = startSearchIndexMethod+seqUniqueStrings(methodIndex).startHighlightStringIndex
    val endHighlightIndexMethod = endSearchIndexMethod + seqUniqueStrings(methodIndex).highlightString.length

    val startSearchIndexAssumption = seqUniqueStrings(assumptionIndex).startSearchStringIndex
    val endSearchIndexAssumption = startSearchIndexAssumption + seqUniqueStrings(assumptionIndex).searchString.length

    val startHighlightIndexAssumption = startSearchIndexAssumption+seqUniqueStrings(assumptionIndex).startHighlightStringIndex
    val endHighlightIndexAssumption = startHighlightIndexAssumption + seqUniqueStrings(assumptionIndex).highlightString.length

    if(startSearchIndexMethod <= startSearchIndexAssumption && endSearchIndexMethod <= startSearchIndexAssumption && endSearchIndexAssumption >= endSearchIndexMethod){
      true
    }
    else if(startHighlightIndexMethod <= startHighlightIndexAssumption && endHighlightIndexMethod <= startHighlightIndexAssumption && endHighlightIndexAssumption >= endHighlightIndexMethod){
      true
    }
    else if(startSearchIndexAssumption <= startSearchIndexMethod && endSearchIndexAssumption <= startSearchIndexMethod && endSearchIndexAssumption <= endSearchIndexMethod){
      true
    }else if(startHighlightIndexAssumption <= startHighlightIndexMethod && endHighlightIndexAssumption <= startHighlightIndexMethod && endHighlightIndexAssumption <= endHighlightIndexMethod){
      true
    }
    else {
      false
    }
  }

  def escapeSearchString(searchString: String): String = {
    val search = searchString.replaceAll(" ", "").map(m => "\\Q"+m+"\\E"+"[\\-\\n\\r\\.]{0,3}[\\s]*").mkString("")
    if(searchString.length <= ALLOWED_MAX_LENGTH_IN_WORD_MATCH || searchString.contains(" ")){
      "(?i)(\\b"+search+"\\b)"
    } else {
      "(?i)("+search+")"
    }
  }

  def getUniqueStringsForSearchTerms(highlightTerms: Map[Color, List[String]]): Iterable[PDFHighlightInstruction] = {
		highlightTerms.flatMap {
			case (color, patterns) => patterns.map(pattern => {

        val allIndicesOfThesePatterns : Iterator[Int] = escapeSearchString(pattern).r.findAllMatchIn(txt).map(_.start)

        val substringIndices: Iterator[(Int, Int)] = allIndicesOfThesePatterns.map(index => {
          extractSmallestBoundaryForSingleMatch(pattern, index)
        })

				val substrings = substringIndices.map(i => txt.substring(i._1, i._2))
				substrings.map(substring => {

          //TODO: What if the searchStringMatch contains two times the word to highlight? which one is to highlight?
          try {
            val searchStringMatch = escapeSearchString(substring).r.findFirstMatchIn(txt).get
            val start = if (escapeSearchString(pattern).r.findFirstMatchIn(searchStringMatch.matched).isDefined) {
              escapeSearchString(pattern).r.findFirstMatchIn(searchStringMatch.matched).get.start
            } else {
              0
            }
            PDFHighlightInstruction(color, substring, pattern, searchStringMatch.start, start)
          }catch {
            case e: Exception => {
              logger.error("Cannot find term " + substring + " in pdf "+ pdfPath,e)
              null
            }
          }
        })

      })
		}.flatten
	}

  def isSmallestMatch(it: Int, indexPosition: Int, inputStringLength: Int): Int = {
    val subTxt = txt.substring(Math.max(0, indexPosition - it), Math.min(txt.length, indexPosition + inputStringLength + it))
    if(escapeSearchString(subTxt).r.findAllMatchIn(txt).length == 1){
      it
    }else {
      isSmallestMatch(it+1, indexPosition, inputStringLength)
    }
  }

  def extractSmallestBoundaryForSingleMatch(inputString: String, indexPosition: Int): (Int, Int) = {

    val it = isSmallestMatch(0, indexPosition, inputString.length)
    (Math.max(0, indexPosition - it), Math.min(txt.length, indexPosition + inputString.length + it))

  }
}


/**
 * Created by pdeboer on 16/06/15.
 */
class PDFHighlight(val pdfPath: String, val instructions: List[PDFHighlightInstruction]) extends LazyLogging {


  def escapeSearchString(searchString: String): String = {
    val search = searchString.replaceAll(" ", "").map(m => "\\Q"+m+"\\E"+"[\\-\\n\\r\\.]{0,3}[\\s]*").mkString("")
    if(searchString.length <= 5 || searchString.contains(" ")){
      "(?i)(\\b"+search+"\\b)"
    } else {
      "(?i)("+search+")"
    }
  }

	/**
	 * taken from Mattia's code and adapted
	 */
	def highlight(): Array[Byte] = {
    try {
      val file = pdfPath
      val parser: PDFParser = new PDFParser(new FileInputStream(file))
      parser.parse()
      val pdDoc: PDDocument = new PDDocument(parser.getDocument)

      val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
      pdfHighlight.setLineSeparator(" ")
      pdfHighlight.initialize(pdDoc)

      instructions.foreach(i => {

        val patterns = List(i.searchString, i.highlightString).map(s => Pattern.compile(escapeSearchString(s)))

        pdfHighlight.highlight(patterns.head, patterns(1), i.color)
      })


      val byteArrayOutputStream = new ByteArrayOutputStream()

			if (pdDoc != null) {
				pdDoc.save(byteArrayOutputStream)
				pdDoc.close()
			}
			if (parser.getDocument != null) {
				parser.getDocument.close()
			}


		  byteArrayOutputStream.toByteArray()
    } catch {
      case e: Exception => {
        logger.error(s"Cannot store highlighted version of pdf: $pdfPath.", e)
        Array.empty[Byte]
      }
    }
	}
}