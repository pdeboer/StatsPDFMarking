package queries

import java.util.regex.Pattern

import input.bmc.BMCDAL
import input.bmc.BMCDAL.DBPaperBody

import scala.io.Source

/**
 * Created by pdeboer on 30/07/15.
 */
object MethodOccurrences extends App {
	val occurrences = Source.fromFile("methodlist.csv").getLines().toList.par.map(l => {
		val terms = l.split(",").map(_.trim())
		val occ = terms.flatMap(t => {
			val targetTerms = if (t.length < 7) addWordBoundaries(t) else List(t)
			targetTerms.flatMap(tt => BMCDAL.getTermOccurrenceCount(tt).map(o => PaperOccurrence(o)(countOccurrences(t, o.body))))
		})
		val sum: Int = occ.groupBy(_.dbp).map(_._2.map(_.occurrences).sum).sum
		s"$l	${occ.toSet.size}	$sum"
	})

	def countOccurrences(needle: String, haystack: String): Int = {
		Pattern.quote(needle).r.findAllMatchIn(haystack).length
	}

	def addWordBoundaries(t: String): List[String] = {
		val thingsToAddForSmallWords = " -.;,!(){}[]:".map(_.toString).toList
		thingsToAddForSmallWords.flatMap(before => {
			thingsToAddForSmallWords.map(after => {
				before + t + after
			})
		})
	}

	case class PaperOccurrence(dbp: DBPaperBody)(val occurrences: Int)

	occurrences.foreach(println)
}
