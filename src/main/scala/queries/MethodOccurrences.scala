package queries

import input.bmc.BMCDAL

import scala.io.Source

/**
 * Created by pdeboer on 30/07/15.
 */
object MethodOccurrences extends App {

	val occurrences = Source.fromFile("methodlist.csv").getLines().toList.par.map(l => {
		val terms = l.split(",").map(_.trim())
		val occ = terms.flatMap(t => {
			val targetTerms = if (t.length < 7) addWordBoundaries(t) else List(t)
			targetTerms.flatMap(BMCDAL.getTermOccurrenceCount)
		}).toSet
		s"$l	${occ.size}"
	})

	def addWordBoundaries(t: String): List[String] = {
		val thingsToAddForSmallWords = " -.;,!(){}[]:".map(_.toString).toList
		thingsToAddForSmallWords.flatMap(before => {
			thingsToAddForSmallWords.map(after => {
				before + t + after
			})
		})
	}

	occurrences.foreach(println)
}
