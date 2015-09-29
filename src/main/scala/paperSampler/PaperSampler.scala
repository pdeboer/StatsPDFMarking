package paperSampler

import java.io.{File, FilenameFilter}

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.LazyLogging
import pdf.PDFTextExtractor

import scala.io.Source

/**
 * Created by mattia on 21.09.15.
 */
object PaperSampler extends App with LazyLogging {

  val allJournalsDir = args(0)
  val PERCENT = args(1).toDouble
  val distributionOverAllJournals = args(2).toBoolean

  logger.debug(s"Journals DIR: $allJournalsDir")
  val journalsToPdfs = new File(allJournalsDir).listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = new File(dir, name).isDirectory
  }).map(journalDir => journalDir.getName -> journalDir.listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")}).toList).toMap

  val journals = journalsToPdfs.keys.toList

  // corpus contains the occurrences of every methods for each paper
  var corpus = new PaperContainer()

  val termLoader : List[List[String]] = Source.fromFile("methodlist_full.csv", "UTF-8").getLines().map(l => {
    l.split(",").map(_.trim()).toList
  }).toList

  journalsToPdfs.par.foreach(journal => {
    journal._2.par.foreach(pdf => {
      val pdfTxt = PDFTextExtractor.extract(pdf.getAbsolutePath)
      val methods : Map[String, Int] = termLoader.map(terms => {
        val method = terms.head
        val occurrences : Int = terms.flatMap(term => pdfTxt.map(pageTxt => PDFTextExtractor.countAllOccurrences(term, pageTxt))).sum
        method -> occurrences
      }).toMap

      corpus.add(Some(Paper(pdf.getPath, journal._1, methods)))
    })
  })

  createCorpusCSVFile("corpus", corpus)
  logger.debug("Corpus csv created")

  var distribution : Map[String, Int] = if(distributionOverAllJournals){
    val dist = termLoader.map(terms => {
      val method = terms.head
      method -> Math.floor(corpus.getOccurrenceOfMethodOverAllPapers(method)*PERCENT / 100.0).toInt
    }).toMap
    logger.debug("Distribution defined")
    dist
  }else{
    Map.empty[String, Int]
  }

  journals.foreach(journal => {

    if(!distributionOverAllJournals){
      distribution = termLoader.map(terms => {
        val method = terms.head
        method -> Math.floor(corpus.getOccurrenceOfMethodOverAllPapersInJournal(method, journal)*PERCENT / 100.0).toInt
      }).toMap
      logger.debug("Distribution defined")
    }

    //corpus.get.foreach(d => logger.debug(d._1 + " ->: container: " +corpus.countPapersContainingMethod(d._1) + ", total occurrences: " + corpus.getOccurrenceOfMethodOverAllPapers(d._1) ))

    var usedPapers = new PaperContainer()

    logger.debug(s"Start algorithm for journal $journal...")

    usedPapers = findBestPath(corpus, journal)

    /*var tmpDistance: Double = 10000.0

    var tmpUsedPapers: PaperContainer = usedPapers.copy
    var tmpCorpus: PaperContainer = corpus.copy

    while(!usedPapers.sameAs(distribution, journal)){
      val distance = calcDistance(usedPapers, journal)

      if(tmpDistance > distance){
        tmpDistance = distance
        tmpUsedPapers = usedPapers.copy
        tmpCorpus = corpus.copy

        createCSVFile("tmpUsedPapers", tmpUsedPapers)
        logger.debug(s"Distance: $tmpDistance")
      }else {
        usedPapers = tmpUsedPapers
        corpus = tmpCorpus
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
    }*/

    createCSVFile("usedPapers", usedPapers)
    logger.debug("Used Paper csv created")

  })

  def calcDistance(papers: PaperContainer, journal: String) : Double = {
    Math.sqrt(distribution.map(d => {
      Math.pow(papers.getOccurrenceOfMethodOverAllPapersInJournal(d._1, journal) - d._2, 2.0)
    }).sum)
  }

  def findBestPath(papers: PaperContainer, journal: String) : PaperContainer = {
    val allPdfs : List[Paper] = papers.get.flatMap(_._2).toList.distinct
    val allPapersPermutations : List[List[Paper]] = allPdfs.toSet.subsets.map(_.toList).toList

    val sol : Map[List[Paper], Double] = allPapersPermutations.par.map(perm => {
      val container = new PaperContainer(perm.toSet)
      perm -> calcDistance(container, journal)
    }).seq.toMap

    val min = sol.minBy(_._2)
    val solution = new PaperContainer(min._1.toSet)
    logger.debug(s"Found solution with distance ${min._2} and ${min._1.size} papers")
    solution
  }

  def createCSVFile(filename: String, papers: PaperContainer): Unit = {
    val wr = CSVWriter.open(new File("./"+filename + allJournalsDir.replaceAll("\\Q..\\E", "").replaceAll("/", "_") + ".csv"))
    val sequMeth1 = termLoader.toSeq.map(_.head)
    wr.writeRow("Paper" +: sequMeth1)
    val allPdfs: List[String] = papers.get.flatMap(_._2.map(_.path)).toList.distinct
    allPdfs.foreach(paper => {
      wr.writeRow(paper +: sequMeth1.map(method => papers.getOccurrenceOfMethodForPaper(paper, method)))
    })
    wr.close()
  }

  def createCorpusCSVFile(filename: String, container: PaperContainer): Unit = {
    val wr = CSVWriter.open(new File("./"+filename + allJournalsDir.replaceAll("\\Q..\\E", "").replaceAll("/", "_") + ".csv"))
    val sequMeth1 = termLoader.toSeq.map(_.head)
    wr.writeRow("Method" +: journals)
    sequMeth1.foreach(method => {
      wr.writeRow(method +: journals.map(journal => container.getOccurrenceOfMethodOverAllPapersInJournal(method, journal)))
    })
    wr.close()
  }
}
