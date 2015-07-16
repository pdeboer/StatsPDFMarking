package highlighting

import java.awt.Color
import java.io.{ByteArrayOutputStream, File, FileInputStream}
import java.util.regex.Pattern

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper

import scala.collection.immutable.Iterable

case class PDFHighlightInstruction(color: Color, searchString: String, highlightString: String)

object PDFTextExtractor {
	def extract(pdfPath: String) = {
		val doc = PDDocument.load(new File(pdfPath))
		val stripper = new PDFTextStripper()
		stripper.getText(doc)
	}
}

class PDFPermuter(pdfPath: String) {
	lazy val txt = PDFTextExtractor.extract(pdfPath)

	def permuteForEachCombinationOf(permutationDefinition: Map[Color, List[String]]): Iterable[PDFHighlight] = {
		val uniqueStrings = getUniqueStringsForSearchTerms(permutationDefinition)
		val uniquePairs = getUniquePairsForSearchTerms(uniqueStrings)

		uniquePairs.map(p => new PDFHighlight(pdfPath, List(p._1, p._2)))
	}

  def getUniquePairsForSearchTerms(uniqueStrings: Iterable[PDFHighlightInstruction]): Iterable[(PDFHighlightInstruction,PDFHighlightInstruction)] = {

    var list = List.empty[(PDFHighlightInstruction, PDFHighlightInstruction)]
    val seqUniqueStrings = uniqueStrings.toSeq

    for(i <- 1 to seqUniqueStrings.length-1) {
      for(j <- 0 to i-1) {
        list ::= (seqUniqueStrings(i) -> seqUniqueStrings(j))
      }
    }

    list
  }

	def getUniqueStringsForSearchTerms(highlightTerms: Map[Color, List[String]]): Iterable[PDFHighlightInstruction] = {
		highlightTerms.flatMap {
			case (color, patterns) => patterns.map(p => {
				val allIndicesOfThesePatterns = (0 until txt.length).filter(txt.startsWith(p, _))
				val charsToTakeFromLeftAndRight = 20
				val substringIndices = allIndicesOfThesePatterns.map(i => (Math.max(0, i - charsToTakeFromLeftAndRight), Math.min(txt.length, i + p.length + charsToTakeFromLeftAndRight)))
				val substrings = substringIndices.map(i => txt.substring(i._1, p.length + i._2))
				substrings.map(s => PDFHighlightInstruction(color, s, p))
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