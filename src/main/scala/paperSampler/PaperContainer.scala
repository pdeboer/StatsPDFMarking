package paperSampler

import scala.util.Random

/**
 * Created by mattia on 21.09.15.
 */
class PaperContainer {

  def add(p: Option[Paper]) = {
    if(p.isDefined){
      p.get.methods.foreach(m => {
        if(p.get.methods.getOrElse(m._1, 0) > 0){
          methodPapers += m._1 -> (methodPapers.getOrElse(m._1, Set.empty[Paper]) ++ Set[Paper](p.get))
        }
      })
    }
  }

  private var methodPapers : Map[String, Set[Paper]] = Map.empty[String, Set[Paper]]

  def diff(distribution: Map[String, Int]) : Boolean = {
    distribution.forall(d => getOccurrenceOfMethodOverAllPapers(d._1) == d._2)
  }

  override def toString() : String = {
    methodPapers.map(m => m._1 + " -> " +  m._2.map(paper => paper.path + " -> occurrences: " + paper.methods.getOrElse(m._1, 0)).head).mkString("\n\n")
  }

  def removeRandomPaper(method: String) : Option[Paper] = {
    val shuffled : List[Paper] = Random.shuffle(methodPapers.getOrElse(method, Set.empty[Paper]).filter(paper => paper.methods.get(method).get > 0).toList)
    val toRemove : Option[Paper] = shuffled.headOption

    methodPapers.par.map(m => {
      m._1 -> m._2.filterNot(_.path.equalsIgnoreCase(toRemove.get.path))
    })

    toRemove
  }

  def get: Map[String, Set[Paper]] = methodPapers

  def copy : PaperContainer = {
    val ret = new PaperContainer
    ret.methodPapers ++= methodPapers
    ret
  }

  def getOccurrenceOfMethodForPaper(paperPath: String, method: String) : Int = {
    val possiblePapers = methodPapers.get(method)
    if(possiblePapers.isDefined){
      val found = possiblePapers.get.find(_.path.equalsIgnoreCase(paperPath))
      if(found.isDefined){
        found.get.methods.get(method).get
      } else {
        0
      }
    }else {
      0
    }
  }

  def countPapersContainingMethod(method: String) : Int = {
    methodPapers.getOrElse(method, List.empty[Paper]).count(_.methods.getOrElse(method, 0) > 0)
  }

  def getOccurrenceOfMethodOverAllPapers(method: String) : Int = {
    methodPapers.getOrElse(method, List.empty[Paper]).map(_.methods.getOrElse(method, 0)).sum
  }

}

case class Paper(path: String, methods: Map[String, Int])
