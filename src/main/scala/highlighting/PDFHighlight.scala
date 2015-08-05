package highlighting

import java.awt.Color
import java.io.{ByteArrayOutputStream, FileInputStream}
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument

import scala.collection.immutable.Iterable

case class PDFHighlightInstruction(color: Color, searchString: String, highlightString: String, startSearchStringIndex: Int, startHighlightStringIndex: Int)

object PDFTextExtractor {
  def extract(pdfPath: String): String = {
    try {
      val parser: PDFParser = new PDFParser(new FileInputStream(pdfPath))
      parser.parse()
      val pdDoc: PDDocument = new PDDocument(parser.getDocument)

      val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
      pdfHighlight.setLineSeparator(" ")
      pdfHighlight.initialize(pdDoc)

      val txt = (0 to pdDoc.getNumberOfPages).map(pdfHighlight.textCache.getText(_)).mkString("\n")
      pdDoc.close()

      txt
    } catch {
      case e: Exception => throw e
    }
	}
}

class PDFPermuter(pdfPath: String) {

  val ALLOWED_MAX_LENGTH_IN_WORD_MATCH = 5

	lazy val txt = PDFTextExtractor.extract(pdfPath)

	def permuteForEachCombinationOf(permutationDefinition: Map[Color, List[String]]): Iterable[PDFHighlight] = {
		val uniqueStrings = getUniqueStringsForSearchTerms(permutationDefinition)
		val uniquePairs = getUniquePairsForSearchTerms(uniqueStrings)

		uniquePairs.map(p => new PDFHighlight(pdfPath, List(p._1, p._2)))
	}

  def getUniquePairsForSearchTerms(uniqueStrings: Iterable[PDFHighlightInstruction]): Iterable[(PDFHighlightInstruction,PDFHighlightInstruction)] = {
    var uniquePairs = List.empty[(PDFHighlightInstruction, PDFHighlightInstruction)]
    val seqUniqueStrings = uniqueStrings.toSeq

    for(i <- 0 to seqUniqueStrings.length-1) {
      for(j <- i to seqUniqueStrings.length-1) {

        if(isUniquePairValidCandidate(seqUniqueStrings(i), seqUniqueStrings(j))) {
          val cleanCandidate = cleanUniquePairsCandidate(seqUniqueStrings, i, j)
          if(cleanCandidate.isDefined){
            uniquePairs ::= cleanCandidate.get
          }
        }
      }
    }
    uniquePairs
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

    !method.highlightString.equals(assumption.highlightString) &&
      !method.searchString.equals(assumption.searchString) &&
      terms.methodsAndSynonyms.exists(m => method.highlightString.contains(m)) &&
      terms.assumptionsAndSynonyms.exists(a => assumption.highlightString.contains(a) &&
        method.color != assumption.color)
  }

  def isHighlightAssumptionOutsideMethod(seqUniqueStrings: Seq[PDFHighlightInstruction],  methodIndex: Int, assumptionIndex: Int): Boolean = {
    val startIndexMethod = seqUniqueStrings(methodIndex).startSearchStringIndex
    val endIndexMethod = startIndexMethod + seqUniqueStrings(methodIndex).searchString.length

    val startIndexAssumption = seqUniqueStrings(assumptionIndex).startSearchStringIndex
    val startIndexHighlightAssumption = startIndexAssumption +
      escapeSearchString(seqUniqueStrings(assumptionIndex).highlightString).r.findFirstMatchIn(
        txt.substring(startIndexAssumption, startIndexAssumption + seqUniqueStrings(assumptionIndex).searchString.length)).get.start
    
    startIndexHighlightAssumption < startIndexMethod | startIndexHighlightAssumption > endIndexMethod
  }

  def escapeSearchString(searchString: String): String = {
    "(?i)(\\Q"+searchString+"\\E)"
  }

  def getUniqueStringsForSearchTerms(highlightTerms: Map[Color, List[String]]): Iterable[PDFHighlightInstruction] = {
		highlightTerms.flatMap {
			case (color, patterns) => patterns.map(pattern => {

        val allIndicesOfThesePatterns : Iterator[Int] =
          if(pattern.length <= ALLOWED_MAX_LENGTH_IN_WORD_MATCH){
            ("(?i)(\\b"+pattern+"\\b)").r.findAllMatchIn(txt).map(_.start)
          } else {
            escapeSearchString(pattern).r.findAllMatchIn(txt).map(_.start)
          }

        val substringIndices: Iterator[(Int, Int)] = allIndicesOfThesePatterns.map(index => {
          extractSmallestBoundaryForSingleMatch(pattern, index)
        })

				val substrings = substringIndices.map(i => txt.substring(i._1, i._2))
				substrings.map(substring => {
          val searchStringMatch = escapeSearchString(substring).r.findFirstMatchIn(txt).get
          PDFHighlightInstruction(color, substring, pattern, searchStringMatch.start, 
            escapeSearchString(pattern).r.findFirstMatchIn(searchStringMatch.group(0)).get.start)
        })

      })
		}.flatten
	}

  def extractSmallestBoundaryForSingleMatch(inputString: String, indexPosition: Int): (Int, Int) = {
    var it = 0
    while (escapeSearchString(txt.substring(Math.max(0, indexPosition - it), Math.min(txt.length, indexPosition + inputString.length + it)))
      .r.findAllIn(txt).length != 1) {
      it += 1
    }
    (Math.max(0, indexPosition - it), Math.min(txt.length, indexPosition + inputString.length + it))
  }
}


/**
 * Created by pdeboer on 16/06/15.
 */
class PDFHighlight(val pdfPath: String, val instructions: List[PDFHighlightInstruction]) extends LazyLogging {

	/**
	 * taken from Mattia's code and adapted
	 */
	def highlight(): Array[Byte] = {
		val file = pdfPath
		val parser: PDFParser = new PDFParser(new FileInputStream(file))
		parser.parse()
		val pdDoc: PDDocument = new PDDocument(parser.getDocument)

		val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
		pdfHighlight.setLineSeparator(" ")
		pdfHighlight.initialize(pdDoc)

    instructions.foreach(i => {

			val patterns = List(i.searchString, i.highlightString).map(s => Pattern.compile(Pattern.quote(s), Pattern.CASE_INSENSITIVE))

			pdfHighlight.highlight(patterns.head, patterns(1), i.color)
		})


		val byteArrayOutputStream = new ByteArrayOutputStream()
		try {
			if (pdDoc != null) {
				pdDoc.save(byteArrayOutputStream)
				pdDoc.close()
			}
			if (parser.getDocument != null) {
				parser.getDocument.close()
			}
		}
		catch {
			case e: Exception => {
				e.printStackTrace
			}
		}
		byteArrayOutputStream.toByteArray()
	}
}