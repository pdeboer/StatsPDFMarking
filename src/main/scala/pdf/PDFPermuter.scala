package pdf

import java.awt.Color
import java.io.{File, FileInputStream}
import java.util.regex.Pattern

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, PDFHighlighter, TextHighlight}
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper
import utils.Utils

import scala.collection.immutable.Iterable

/**
 * Created by mattia on 02.09.15.
 */
case class PDFHighlightInstruction(color: Color, searchString: String, highlightString: String, startSearchStringIndex: Int, startHighlightStringIndex: Int, pageNr: Int)

case class Permutation(groupName: String, methodIndex: String, snippetPath: String, pdfPath: String, methodOnTop: Boolean, relativeTop: Double = 0, relativeBottom: Double = 0)

object PDFTextExtractor extends LazyLogging {
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

	def extractTextAsString(pdfPath: String): String = {
		try {
			val pdDoc: PDDocument = PDDocument.load(new File(pdfPath))

			val stripper = new PDFTextStripper()
			val txt = stripper.getText(pdDoc)
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

	def countAllOccurrences(method: String, txt: String): Int = {
		val txtLower = txt.toLowerCase
		Utils.escapeSearchString(method.toLowerCase).map(_.r.findAllMatchIn(txtLower).length).sum
	}
}

class PDFPermuter(pdfPath: String) extends LazyLogging {

	lazy val txt: List[String] = PDFTextExtractor.extract(pdfPath)
	val config = ConfigFactory.load()
	val MULTIVARIATE_MAX_DISTANCE = config.getInt("highlighter.multivariateMaxDistance")

	def findAllMethodsInPaper(permutationDefinition: Map[Color, List[String]]): List[PDFHighlightInstruction] = {
		val uniqueStrings = getUniqueStringsForSearchTerms(permutationDefinition)
		uniqueStrings.toList
	}

	def getUniquePairsForSearchTerms(methodsList: List[PDFHighlightInstruction], assumptionsList: List[PDFHighlightInstruction]): List[PDFHighlighter] = {
		assumptionsList.map(p => new PDFHighlighter(pdfPath, methodsList.toList ::: List(p)))
	}

	def cleanUniquePairsCandidate(seqUniqueStrings: Seq[PDFHighlightInstruction], methodIndex: Int, assumptionIndex: Int): Option[(PDFHighlightInstruction, PDFHighlightInstruction)] = {
		if (isHighlightAssumptionOutsideMethod(seqUniqueStrings, methodIndex, assumptionIndex)) {
			Some(seqUniqueStrings(methodIndex), seqUniqueStrings(assumptionIndex))
		} else {
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

		val valid =
			(startSearchIndexMethod <= startSearchIndexAssumption && endSearchIndexMethod <= startSearchIndexAssumption && endSearchIndexAssumption >= endSearchIndexMethod) ||
				(startHighlightIndexMethod <= startHighlightIndexAssumption && endHighlightIndexMethod <= startHighlightIndexAssumption && endHighlightIndexAssumption >= endHighlightIndexMethod) ||
				(startSearchIndexAssumption <= startSearchIndexMethod && endSearchIndexAssumption <= startSearchIndexMethod && endSearchIndexAssumption <= endSearchIndexMethod) ||
				(startHighlightIndexAssumption <= startHighlightIndexMethod && endHighlightIndexAssumption <= startHighlightIndexMethod && endHighlightIndexAssumption <= endHighlightIndexMethod)

		valid
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

	def getUniqueStringsForSearchTerms(highlightTerms: Map[Color, List[String]]): Iterable[PDFHighlightInstruction] = {

		highlightTerms.flatMap {
			case (color, patterns) => patterns.flatMap(highlightPattern => {
				txt.zipWithIndex.flatMap(pageTxt => {
					val allIndicesOfThesePatterns: List[Int] = Utils.escapeSearchString(highlightPattern).flatMap(_.r.findAllMatchIn(pageTxt._1).map(_.start))

					val indexesToDiscard: List[Int] = identifyIndexSpecialCases(highlightPattern, pageTxt, allIndicesOfThesePatterns)

					val substringIndices: List[(Int, Int)] = extractSubstringIndicesWithoutInvalidCases(highlightPattern, allIndicesOfThesePatterns, indexesToDiscard, pageTxt._1)
					val substrings = substringIndices.map(i => pageTxt._1.substring(i._1, i._2))

					val pdfHighlightInstructions = substrings.map((searchString: String) => createPDFHighlightInstructionForSubstring(color, highlightPattern, searchString, pageTxt))
					pdfHighlightInstructions.filter(_.isDefined).map(_.get)
				})
			})
		}
	}

	def createPDFHighlightInstructionForSubstring(color: Color, highlightPattern: String, searchString: String, pageTxt: (String, Int)): Option[PDFHighlightInstruction] = {
		try {
			val searchStringMatch = Pattern.quote(searchString.toLowerCase).r.findFirstMatchIn(pageTxt._1.toLowerCase).get
			val startHighlightStringIndex = Pattern.quote(highlightPattern.toLowerCase).r.findFirstMatchIn(searchStringMatch.matched.toLowerCase).map(_.start).get

			Some(PDFHighlightInstruction(color, searchString, highlightPattern, searchStringMatch.start, startHighlightStringIndex, pageTxt._2))
		} catch {
			case e: Exception => {
				logger.error("Cannot find term " + highlightPattern + " in pdf " + pdfPath, e)
				None
			}
		}
	}

	def extractSubstringIndicesWithoutInvalidCases(pattern: String, allIndicesOfThesePatterns: List[Int], indexesToDiscard: List[Int], pageTxt: String): List[(Int, Int)] = {
		allIndicesOfThesePatterns.filterNot(indexesToDiscard.contains(_)).map(index => {
			extractSmallestBoundaryForSingleMatch(pattern, index, pageTxt)
		})
	}

	def identifyIndexSpecialCases(pattern: String, pageTxt: (String, Int), allIndicesOfThesePatterns: List[Int]): List[Int] = {
		if (pattern.equalsIgnoreCase("ANOVA") || pattern.equalsIgnoreCase("analysis of variance")) {
			val indexes = Utils.escapeSearchString("multivariate").flatMap(_.r.findAllMatchIn(pageTxt._1).map(_.start))
			if (indexes.nonEmpty) {
				indexes.flatMap(i => allIndicesOfThesePatterns.map(j => (i, j))).filter(m => Math.abs(m._1 - m._2) <= MULTIVARIATE_MAX_DISTANCE).map(_._2).toList
			} else {
				List.empty[Int]
			}
		} else {
			List.empty[Int]
		}
	}

	def isSmallestMatch(it: Int, indexPosition: Int, inputStringLength: Int, pageTxt: String): Int = {
		val subTxt = pageTxt.substring(Math.max(0, indexPosition - it), Math.min(pageTxt.length, indexPosition + inputStringLength + it))
		if (Pattern.quote(subTxt.toLowerCase).r.findAllMatchIn(txt.mkString("").toLowerCase).length == 1) {
			it
		} else {
			isSmallestMatch(it + 1, indexPosition, inputStringLength, pageTxt)
		}
	}

	def extractSmallestBoundaryForSingleMatch(inputString: String, indexPosition: Int, pageTxt: String): (Int, Int) = {
		try {
			val numberOfCharsToIdentifyString = isSmallestMatch(0, indexPosition, inputString.length, pageTxt)

			val selectedTxt = pageTxt.substring(Math.max(0, indexPosition - numberOfCharsToIdentifyString), Math.min(pageTxt.length, indexPosition + inputString.length + numberOfCharsToIdentifyString))

			val spaces = selectedTxt.count(_ == ' ')
			val dashes = selectedTxt.count(_ == '-') + selectedTxt.count(_ == '–') + selectedTxt.count(_ == '—') + selectedTxt.count(_ == '―')

			(Math.max(0, indexPosition - numberOfCharsToIdentifyString), Math.min(pageTxt.length, indexPosition + inputString.length + spaces + dashes + numberOfCharsToIdentifyString + 1))

		} catch {
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
