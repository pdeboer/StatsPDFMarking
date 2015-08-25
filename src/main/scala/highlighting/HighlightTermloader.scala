package highlighting

import scala.collection.mutable
import scala.io.Source

/**
 * Created by pdeboer on 16/06/15.
 */
class HighlightTermloader {

  lazy val deltas = {

    val methodToDelta = Source.fromFile("deltas.csv").getLines().map(l => {
      val cols = l.split(",")
      (cols(0), cols.drop(1).head.toInt)
    }).toList

    methodToDelta
  }

  lazy val terms = {

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

	def termNames = terms.map(_.name).toList
	def termSynonyms = terms.flatMap(t => t.synonyms).toList
	def termAssumptions = terms.flatMap(t => t.assumptions.map(a => a.name)).toList
	def termAssumptionSynonyms = terms.flatMap(t => t.assumptions.flatMap(a => a.synonym)).toList
	def methodsAndSynonyms = termNames ::: termSynonyms
	def assumptionsAndSynonyms = termAssumptions ::: termAssumptionSynonyms
	def allTerms = termNames ::: termSynonyms ::: termAssumptions ::: termAssumptionSynonyms

  def getDeltaForMethod(method: String) : Int = {
    try {
      deltas.filter(_._1.equalsIgnoreCase(method)).head._2
    }catch{
      case e: Exception => 5000
    }
  }

  def getMethodFromSynonymOrMethod(synonymOrMethod: String) : Option[StatisticalMethod] = {
    terms.find(t => t.name.equalsIgnoreCase(synonymOrMethod) || t.synonyms.contains(synonymOrMethod))
  }

  def getMethodAndSynonymsFromMethodName(method: String): Option[StatisticalMethod] = {
    terms.find(m => m.name.equalsIgnoreCase(method))
  }

}

case class StatisticalMethod(name: String, synonyms: List[String], var assumptions: List[StatisticalAssumption])

case class StatisticalAssumption(name: String, synonym: List[String])