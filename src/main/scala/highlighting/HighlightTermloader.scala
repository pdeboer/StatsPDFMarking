package highlighting

import scala.collection.mutable
import scala.io.Source

/**
 * Created by pdeboer on 16/06/15.
 */
object HighlightTermloader {
	def load() = {
		val assumptionsInCSV = Source.fromFile("assumptions.csv").getLines().map(l => {
			val cols = l.split(",")
			StatisticalAssumption(cols(0), cols.drop(1).toList)
		}).toList

		val methodNamesAndSynonyms = Source.fromFile("methods.csv").getLines().map(l => {
			val cols = l.split(",")
			(cols(0), cols.drop(1).toList)
		}).toList
		var methodMap = new mutable.HashMap[String, List[StatisticalAssumption]]()
		Source.fromFile("met2ass.csv").getLines().foreach(l => {
			val cols = l.split(",")

			val assumption = assumptionsInCSV.find(_.name == cols(1)).getOrElse( throw new Exception(cols(1)) )
			methodMap += cols(0) -> (assumption :: methodMap.getOrElse(cols(0), Nil))
		})

		val methods = methodMap.map { case (method, assumptions) =>
			val methodAndSynonym = methodNamesAndSynonyms.find(_._1 == method).get
			StatisticalMethod(methodAndSynonym._1, methodAndSynonym._2, assumptions)
		}

		methods
	}
}

case class StatisticalMethod(name: String, synonyms: List[String], var assumptions: List[StatisticalAssumption])

case class StatisticalAssumption(name: String, synonym: List[String])