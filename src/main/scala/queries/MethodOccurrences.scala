package queries

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.regex.Pattern

import input.bmc.{DBPaperBody, EuGenDAL}

import scala.io.Source

/**
 * Created by pdeboer on 30/07/15.
 */
object MethodOccurrences extends App {
	val occurrences: List[MethodOccurrence] = Source.fromFile("methodlist.csv").getLines().toList.par.map(l => {
		val terms = l.split(",").map(_.trim())
		val papersWithTermVariations = terms.flatMap(t => {
			val targetTerms = if (t.length < 7) addWordBoundaries(t) else List(t)
			targetTerms.flatMap(tt => EuGenDAL.getPapersContainingTerm(tt).map(o => PaperOccurrence(o)(List(tt))))
		})
		val termOccurrences = papersWithTermVariations.groupBy(_.dbp).map {
			case (body, occurenceList) => PaperOccurrence(body)(occurenceList.map(po => po.terms).toList.flatten)
		}

		MethodOccurrence(l, termOccurrences.toList)
	}).toList

	val methodNumbers = occurrences.par.map(mo => {
		//val yearly: List[MethodCountPerYear] = (2009 to 2014).map(year => countOccurrencePerYear(mo, year)).toList
		val overview = countOccurrencePerYear(mo)
		MethodCounts(mo, overview, Nil)
	})
	val paperIDs: Set[Long] = occurrences.flatMap(_.po.map(_.dbp.id)).toSet

	def countOccurrencePerYear(mo: MethodOccurrence, year: Int = -1): MethodCountPerYear = {
		val occurrenceList = if (year == -1) mo else MethodOccurrence(mo.method, mo.po.filter(_.dbp.year == year))
		val stringMatches = occurrenceList.po.foldLeft(0)((s, po) => s + countOccurrences(po.terms, po.dbp.body))
		val numPapers = occurrenceList.po.map(_.dbp).toSet.size
		MethodCountPerYear(year, numPapers, stringMatches)
	}

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

	def copy(source: File, target: File) {
		assert(source.exists() && !target.exists())

		target.createNewFile()
		new FileOutputStream(target) getChannel() transferFrom(
			new FileInputStream(source) getChannel, 0, Long.MaxValue)
	}

	case class MethodCounts(methodOccurrence: MethodOccurrence, overall: MethodCountPerYear, years: List[MethodCountPerYear]) extends Serializable {}

	case class MethodCountPerYear(year: Int, numPapers: Int, numStringMatches: Int) extends Serializable {
		val csvHeader = {
			val yearDescription = if (year == -1) "all" else year + ""
			s"$yearDescription papers,$yearDescription string matches"
		}

		override def toString = numPapers + "," + numStringMatches
	}

	/*
	println(s"Method,${methodNumbers.head.overall.csvHeader},${methodNumbers.head.years.map(_.csvHeader).mkString(",")} ")
	methodNumbers.foreach(mc => println(s"${mc.methodOccurrence},${mc.overall},${mc.years.mkString(",")} "))
	*/

	case class MethodOccurrence(method: String, po: List[PaperOccurrence]) extends Serializable {
		override def toString = method.replaceAll(",", "/")
	}

	paperIDs.par.foreach(p => {
		val fileName = EuGenDAL.getPaperFilename(p)
		val sourceFile = new File("/Users/pdeboer/Documents/phd_local/OpenReviewCrawler/papers/european journal of medical practise/" + fileName)
		val targetFile = new File("/Users/pdeboer/Downloads/papers/eujoupract/" + fileName)

		copy(sourceFile, targetFile)
	})

	case class PaperOccurrence(dbp: DBPaperBody)(val terms: List[String]) extends Serializable
}
