package paperSampler

import java.io.File

import scala.util.Random

/**
 * Created by mattia on 21.09.15.
 */
class PaperContainer {

  def add(p: Option[Paper]) = {
    if(p.isDefined){
      p.get.methods.foreach(m => methodPapers += m._1 -> (methodPapers.getOrElse(m._1, List.empty[Paper]) ::: List[Paper](p.get)))
    }
  }

  private var methodPapers : Map[String, List[Paper]] = Map.empty[String, List[Paper]]

  def diff(distribution: Map[String, Int]) : Boolean = {
    distribution.forall(d => getOccurrenceOfMethodOverAllPapers(d._1) == d._2)
  }

  override def toString() : String = {
    methodPapers.map(m => m._1 + " -> " +  m._2.map(paper => paper.path + " -> occurrences: " + paper.methods.getOrElse(m._1, 0)).head).mkString("\n\n")
  }

  def removeRandomPaper(method: String) : Option[Paper] = {
    val shuffled = Random.shuffle(methodPapers.getOrElse(method, List.empty[Paper]))
    val toRemove = shuffled.headOption
    val rest = shuffled.drop(1)
    methodPapers += method -> rest
    toRemove
  }

  def get: Map[String, List[Paper]] = methodPapers

  def getOccurrenceOfMethodForPaper(paperFile: File, method: String) : Int = {
    val possiblePaper = methodPapers.getOrElse(method, List.empty[Paper]).find(paper => paper.path.equalsIgnoreCase(paperFile.getPath))
    if(possiblePaper.isDefined){
      possiblePaper.get.methods.getOrElse(method, 0)
    } else {
      0
    }
  }

  def getOccurrenceOfMethodOverAllPapers(method: String) : Int = {
    methodPapers.getOrElse(method, List.empty[Paper]).map(_.methods.getOrElse(method, 0)).sum
  }

}

case class Paper(path: String, methods: Map[String, Int])
