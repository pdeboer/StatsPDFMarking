package highlighting

import java.awt.Color
import java.io.{ByteArrayOutputStream, File, FileInputStream}
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper

import scala.collection.immutable.Iterable

case class PDFHighlightInstruction(color: Color, searchString: String, highlightString: String, startSearchStringIndex: Int, startHighlightStringIndex: Int)

object PDFTextExtractor {
	def extract(pdfPath: String) = {
		val doc = PDDocument.load(new File(pdfPath))
		val stripper = new PDFTextStripper()
		stripper.getText(doc)
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
    var list = List.empty[(PDFHighlightInstruction, PDFHighlightInstruction)]
    val seqUniqueStrings = uniqueStrings.toSeq

    for(i <- 0 to seqUniqueStrings.length-1) {
      for(j <- i to seqUniqueStrings.length-1) {

        if( isUniquePairValidCandidate(seqUniqueStrings(i), seqUniqueStrings(j))) {

          val startIndexMethod = seqUniqueStrings(i).startSearchStringIndex
          val endIndexMethod = startIndexMethod + seqUniqueStrings(i).searchString.length
          val startIndexAssumption = seqUniqueStrings(j).startSearchStringIndex
          val startIndexHighlightAssumption = startIndexAssumption+
            addIgnoreCaseAndQuotesToSearchString(seqUniqueStrings(j).highlightString).r.findFirstMatchIn(
              txt.substring(startIndexAssumption, startIndexAssumption+seqUniqueStrings(j).searchString.length)).get.start

          if(isHighlightAssumptionOutsideMethod(startIndexMethod, endIndexMethod, startIndexHighlightAssumption)) {
            list ::= (seqUniqueStrings(i), seqUniqueStrings(j))
          }
        }
      }
    }
    list
  }

  def isUniquePairValidCandidate(method: PDFHighlightInstruction, assumption: PDFHighlightInstruction): Boolean = {
    val terms = new HighlightTermloader

    !method.highlightString.equals(assumption.highlightString) &&
      !method.searchString.equals(assumption.searchString) &&
      terms.methodsAndSynonyms.exists(m => method.highlightString.contains(m)) &&
      terms.assumptionsAndSynonyms.exists(a => assumption.highlightString.contains(a))
  }

  def isHighlightAssumptionOutsideMethod(startIndexMethod: Int, endIndexMethod: Int, startIndexHighlightAssumption: Int): Boolean = {
    startIndexHighlightAssumption < startIndexMethod | startIndexHighlightAssumption > endIndexMethod
  }

  def addIgnoreCaseAndQuotesToSearchString(searchString: String): String = {
    "(?i)(\\Q"+searchString+"\\E)"
  }

  def getUniqueStringsForSearchTerms(highlightTerms: Map[Color, List[String]]): Iterable[PDFHighlightInstruction] = {
		highlightTerms.flatMap {
			case (color, patterns) => patterns.map(p => {
        var allIndicesOfThesePatterns: Set[Int] = Set()

        if(p.length < ALLOWED_MAX_LENGTH_IN_WORD_MATCH){
          ("(?i)(\\b"+p+"\\b)").r.findAllMatchIn(txt).map(m => m.start).foreach(
            index => allIndicesOfThesePatterns += index)
        } else {
          addIgnoreCaseAndQuotesToSearchString(p).r.findAllMatchIn(txt).map(m => m.start).foreach(
            index => allIndicesOfThesePatterns += index)
        }

        val substringIndices: Iterator[(Int, Int)] = allIndicesOfThesePatterns.toIterator.map(i => {
          var it = 1
          while(addIgnoreCaseAndQuotesToSearchString(txt.substring(Math.max(0, i - it), Math.min(txt.length, i + p.length + it))).r.findAllIn(txt).length != 1) {
            it += 1
          }
          (Math.max(0, i - it), Math.min(txt.length, i + p.length + it))
        })

        //val substringIndices = allIndicesOfThesePatterns.map(i => (Math.max(0, i - charsToTakeFromLeftAndRight), Math.min(txt.length, i + p.length + charsToTakeFromLeftAndRight)))

				val substrings = substringIndices.map(i => txt.substring(i._1, i._2))
				substrings.map(s => {
          val searchStringMatch = addIgnoreCaseAndQuotesToSearchString(s).r.findFirstMatchIn(txt).get
          PDFHighlightInstruction(color, s, p, searchStringMatch.start, addIgnoreCaseAndQuotesToSearchString(p).r.findFirstMatchIn(searchStringMatch.group(0)).get.start)
        })
			})
		}.flatten
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
			logger.debug(s"Highlighting color ${i.color} for search pattern ${i.searchString} and highlighting pattern ${i.highlightString}")

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