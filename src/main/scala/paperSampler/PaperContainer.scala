package paperSampler

import com.typesafe.scalalogging.LazyLogging

import scala.util.Random

/**
 * Created by mattia on 21.09.15.
 */
class PaperContainer extends LazyLogging {
	def add(p: Option[Paper]) = {
		if (p.isDefined) {
			p.get.methods.foreach(m => {
				if (p.get.methods.getOrElse(m._1, 0) > 0) {
					this.synchronized {
						methodPapers += m._1 -> (methodPapers.getOrElse(m._1, Set.empty[Paper]) ++ Set[Paper](p.get))
					}
				}
			})
		}
	}

	private var methodPapers: Map[String, Set[Paper]] = Map.empty[String, Set[Paper]]

	def sameAs(distribution: Map[String, Int], journal: String): Boolean = {
		distribution.forall(d => countMethodOccurrences(d._1, journal) == d._2)
	}

	override def toString(): String = {
		methodPapers.map(m => m._1 + " -> " + m._2.map(paper => paper.path + " -> occurrences: " + paper.methods.getOrElse(m._1, 0)).head).mkString("\n\n")
	}

	def removeRandomPaper(method: String, maxOccurrenceOfMethod: Int): Option[Paper] = {
		val possiblePapers = Random.shuffle(methodPapers.getOrElse(method, Set.empty[Paper]).filter(paper => paper.methods.getOrElse(method, 0) > 0
			&& paper.methods.getOrElse(method, 0) <= maxOccurrenceOfMethod))
		val toRemove: Option[Paper] = possiblePapers.headOption
		if (toRemove.isDefined) {
			methodPapers = methodPapers.map(m => {
				m._1 -> m._2.filterNot(_.path.equalsIgnoreCase(toRemove.get.path))
			})
			toRemove
		} else {
			val shuffled = Random.shuffle(methodPapers.getOrElse(method, Set.empty[Paper]).filter(paper => paper.methods.getOrElse(method, 0) > 0))
			val rem: Option[Paper] = shuffled.headOption
			if (rem.isDefined) {
				methodPapers = methodPapers.map(m => {
					m._1 -> m._2.filterNot(_.path.equalsIgnoreCase(rem.get.path))
				})
			} else {
				logger.debug("No paper available")
			}
			rem
		}
	}

	def get: Map[String, Set[Paper]] = methodPapers

	def copy: PaperContainer = {
		val ret = new PaperContainer
		ret.methodPapers ++= methodPapers
		ret
	}

	def countMethodOccurrencesInPaper(targetPaperPath: String, method: String): Int = {
		val papersWithMethod = methodPapers.get(method)
		if (papersWithMethod.isDefined) {
			val found = papersWithMethod.get.find(_.path.equalsIgnoreCase(targetPaperPath))
			if (found.isDefined) {
				found.get.methods.get(method).get
			} else {
				0
			}
		} else {
			0
		}
	}

	def countPapersContainingMethodInJournal(method: String, journal: String): Int = {
		methodPapers.getOrElse(method, List.empty[Paper]).count(f => f.methods.getOrElse(method, 0) > 0 && f.journal.equalsIgnoreCase(journal))
	}

	def countMethodOccurrences(method: String, journal: String = null): Int = {
		val papers = methodPapers.getOrElse(method, List.empty[Paper])
		val papersFromTargetJournal = if (journal == null) papers else papers.filter(_.journal.equalsIgnoreCase(journal))
		papersFromTargetJournal.toList.map(_.methods.getOrElse(method, 0)).sum
	}

}

case class Paper(path: String, journal: String, methods: Map[String, Int])
