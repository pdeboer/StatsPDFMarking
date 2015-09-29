package paperSampler

import com.typesafe.scalalogging.LazyLogging
import paperSampler.PaperSampler._

/**
 * Created by mattia on 29.09.15.
 */
object SamplerAlgorithm extends LazyLogging{

  var journalCorpus : PaperContainer = null

  def findNeededElement(jCorpus: PaperContainer, journal: String) : PaperContainer = {
    journalCorpus = jCorpus.copy

    var usedPapers = new PaperContainer()
    var tmpDistance: Double = 10000.0

    var tmpUsedPapers: PaperContainer = usedPapers.copy
    var tmpCorpus: PaperContainer = journalCorpus.copy

    while(!usedPapers.sameAs(distribution, journal)){
      val distance = calcDistance(usedPapers)

      if(tmpDistance > distance){
        tmpDistance = distance
        tmpUsedPapers = usedPapers.copy
        tmpCorpus = journalCorpus.copy

        SamplerWriter.createCSVFile("tmpUsedPapers", tmpUsedPapers, true, termLoader, journals)
        logger.debug(s"Distance: $tmpDistance")
      }else {
        usedPapers = tmpUsedPapers
        journalCorpus = tmpCorpus
      }

      termLoader.foreach(terms => {
        val method = terms.head

        if(usedPapers.getOccurrenceOfMethodOverAllPapersInJournal(method, journal) < distribution.get(method).get){
          val occurrencesLeft = distribution.get(method).get - usedPapers.getOccurrenceOfMethodOverAllPapersInJournal(method, journal)
          usedPapers.add(corpus.removeRandomPaper(method, occurrencesLeft))
        }
        else if(usedPapers.getOccurrenceOfMethodOverAllPapersInJournal(method, journal) > distribution.get(method).get){
          val surplusOccurrences = usedPapers.getOccurrenceOfMethodOverAllPapersInJournal(method, journal) - distribution.get(method).get
          corpus.add(usedPapers.removeRandomPaper(method, surplusOccurrences))
        }
      })
    }

    usedPapers
  }

  def findBestPath(papers: PaperContainer) : PaperContainer = {
    val allPdfs : List[Paper] = papers.get.flatMap(_._2).toList.distinct
    val allPapersPermutations : List[List[Paper]] = allPdfs.toSet.subsets.map(_.toList).toList

    logger.debug(s"Created ${allPapersPermutations.size} different permutations")

    val sol : Map[List[Paper], Double] = allPapersPermutations.par.map(perm => {
      val container = new PaperContainer()
      perm.par.foreach(p => container.add(Some(p)))
      val dist = calcDistance(container)
      if(dist < 5){
        logger.debug(s"Distance $dist with permutations $perm")
      }
      perm -> dist
    }).seq.toMap

    val min = sol.minBy(_._2)
    val solution = new PaperContainer()
    min._1.par.foreach(p => solution.add(Some(p)))
    logger.debug(s"Found solution with distance ${min._2} and ${min._1.size} papers")
    solution
  }

}
