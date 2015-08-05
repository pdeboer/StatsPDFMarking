package queries

import java.util.regex.Pattern

import ch.uzh.ifi.pdeboer.pplib.process.entities.FileProcessMemoizer
import input.bmc.BMCDAL
import input.bmc.BMCDAL.DBPaperBody

import scala.io.Source

/**
 * Created by pdeboer on 30/07/15.
 */
object MethodOccurrences extends App {
	val occurrences: List[MethodOccurrence] = new FileProcessMemoizer("sqldata").mem("method")(Source.fromFile("methodlist.csv").getLines().toList.par.map(l => {
		val terms = l.split(",").map(_.trim())
		val papersWithTermVariations = terms.flatMap(t => {
			val targetTerms = if (t.length < 7) addWordBoundaries(t) else List(t)
			targetTerms.flatMap(tt => BMCDAL.getPapersContainingTerm(tt).map(o => PaperOccurrence(o)(List(tt))))
		})
		val termOccurrences = papersWithTermVariations.groupBy(_.dbp).map {
			case (body, occurenceList) => PaperOccurrence(body)(occurenceList.map(po => po.terms).toList.flatten)
		}

		MethodOccurrence(l, termOccurrences.toList)
	}).toList)


	val methodNumbers = occurrences.map(mo => {
		val stringMatches = mo.po.foldLeft(0)((s, po) => s + countOccurrences(po.terms, po.dbp.body))
		val numPapers = mo.po.map(_.dbp).toSet.size
		MethodCounts(mo, numPapers, stringMatches)
	})


	def countOccurrences(needles: List[String], haystack: String, ignoreCases: Boolean = true): Int = needles.map(n => countOccurrence(n, haystack, ignoreCases)).sum

	def countOccurrence(needle: String, haystack: String, ignoreCases: Boolean = true): Int = {
		val needleIgnoreCase = if (ignoreCases) needle.toLowerCase else needle
		val haystackIgnoreCase = if (ignoreCases) haystack.toLowerCase else haystack
		Pattern.quote(needleIgnoreCase).r.findAllMatchIn(haystackIgnoreCase).length
	}

	def addWordBoundaries(t: String): List[String] = {
		val thingsToAddForSmallWords = " -.;,!(){}[]:".map(_.toString).toList
		thingsToAddForSmallWords.flatMap(before => {
			thingsToAddForSmallWords.map(after => {
				before + t + after
			})
		})
	}

	case class MethodCounts(methodOccurrence: MethodOccurrence, numPapers: Int, numStringMatches: Int) extends Serializable {
		override def toString = s"${methodOccurrence.method.replaceAll(",", "/")},$numPapers,$numStringMatches"
	}

	case class MethodOccurrence(method: String, po: List[PaperOccurrence]) extends Serializable

	case class PaperOccurrence(dbp: DBPaperBody)(val terms: List[String]) extends Serializable

	methodNumbers.foreach(println)
}
