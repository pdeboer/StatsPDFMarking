package highlighting

import scala.collection.mutable
import scala.io.Source

/**
 * Created by pdeboer on 16/06/15.
 */
class HighlightTermloader {

  val deltas = {

    val methodToDelta = Source.fromFile("deltas.csv", "UTF-8").getLines().map(l => {
      val cols = l.split(",")
      (cols(0), cols(1).toInt)
    }).toList

    methodToDelta
  }

  lazy val terms = {

    val assumptionsInCSV = Source.fromFile("assumptions.csv", "UTF-8").getLines().map(l => {
			val cols = l.split(",")
			StatisticalAssumption(cols(0), cols.drop(1).toList)
		}).toList

		val methodNamesAndSynonyms = Source.fromFile("methods.csv", "UTF-8").getLines().map(l => {
			val cols = l.split(",")
			(cols(0), cols.drop(1).toList)
		}).toList

    var methodMap = new mutable.HashMap[String, List[StatisticalAssumption]]()
		Source.fromFile("met2ass.csv", "UTF-8").getLines().foreach(l => {
			val cols = l.split(",")

			val assumption = assumptionsInCSV.find(_.assumptionName == cols(1)).getOrElse( throw new Exception(cols(1)) )
			methodMap += cols(0) -> (assumption :: methodMap.getOrElse(cols(0), Nil))
		})

		val methods = methodMap.map { case (method, assumptions) =>
			val methodAndSynonym = methodNamesAndSynonyms.find(_._1 == method).get
			StatisticalMethod(methodAndSynonym._1, methodAndSynonym._2, assumptions)
		}

		methods
	}

	def methods = terms.map(_.methodName).toList
	def termSynonyms = terms.flatMap(t => t.synonyms).toList
	def termAssumptions = terms.flatMap(t => t.assumptions.map(a => a.assumptionName)).toList
	def termAssumptionSynonyms = terms.flatMap(t => t.assumptions.flatMap(a => a.synonym)).toList
	def methodsAndSynonyms = methods ::: termSynonyms
	def assumptionsAndSynonyms = termAssumptions ::: termAssumptionSynonyms
	def allTerms = methods ::: termSynonyms ::: termAssumptions ::: termAssumptionSynonyms

  def getDeltaForMethod(method: String) : Int = {
    try {
      val value = deltas.find(_._1.equalsIgnoreCase(method)).getOrElse((method , 0))
      value._2
    }catch{
      case e: Exception => 0
    }
  }

  def getMethodAndSynonymsFromMethodName(method: String): Option[StatisticalMethod] = {
    terms.find(m => m.methodName.equalsIgnoreCase(method))
  }

}

case class StatisticalMethod(methodName: String, synonyms: List[String], var assumptions: List[StatisticalAssumption])

case class StatisticalAssumption(assumptionName: String, synonym: List[String])