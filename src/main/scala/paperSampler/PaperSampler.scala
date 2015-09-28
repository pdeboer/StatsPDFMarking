package paperSampler

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.LazyLogging
import input.folder.FolderPDFSource
import pdf.PDFTextExtractor

import scala.io.Source

/**
 * Created by mattia on 21.09.15.
 */
object PaperSampler extends App with LazyLogging {

  val pdfsDir = args(0)
  logger.debug("PDFs DIR: " + pdfsDir)
  val PERCENT = args(1).toDouble

  val pdfs = new FolderPDFSource(pdfsDir).get().toList

  // corpus contains the occurrences of every methods for each paper
  var corpus = new PaperContainer()

  val termLoader : List[List[String]] = Source.fromFile("methodlist_full.csv", "UTF-8").getLines().map(l => {
    l.split(",").map(_.trim()).toList
  }).toList

  pdfs.par.foreach(pdf => {
    val txt = PDFTextExtractor.extractTextAsString(pdf.getAbsolutePath)
    val methods : Map[String, Int] = termLoader.map(terms => {
      val method = terms.head
      val occurrences : Int = terms.map(c => PDFTextExtractor.countAllOccurrences(c, txt)).sum
      method -> occurrences
    }).toMap

    corpus.add(Some(Paper(pdf.getPath, methods)))
  })

  createCSVFile("corpus", corpus)
  logger.debug("Corpus csv created")

  val distribution : Map[String, Int] = termLoader.map(terms => {
    val method = terms.head
    method -> Math.floor(corpus.getOccurrenceOfMethodOverAllPapers(method)*PERCENT / 100.0).toInt
  }).toMap

  logger.debug("Distribution defined")

  //corpus.get.foreach(d => logger.debug(d._1 + " ->: papers: " +corpus.countPapersContainingMethod(d._1) + ", total occurrences: " + corpus.getOccurrenceOfMethodOverAllPapers(d._1) ))

  var usedPapers = new PaperContainer()

  var tmpDistance = calcDistance

  var tmpUsedPapers: PaperContainer = usedPapers.copy
  var tmpCorpus: PaperContainer = corpus.copy

  logger.debug("Start algorithm...")
  logger.debug(s"Distance: $tmpDistance")

  while(!usedPapers.diff(distribution)){
    val distance = calcDistance
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
      if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) < distribution.get(method).get){
        val paper = corpus.removeRandomPaper(method)
        if(paper.isDefined){
          usedPapers.add(paper)
        }else {
          logger.debug(s"ADD to used: Cannot find any free paper containing $method : found=${corpus.getOccurrenceOfMethodOverAllPapers(method)}")
        }
      }else if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) > distribution.get(method).get){
        val paper = usedPapers.removeRandomPaper(method)
        if(paper.isDefined){
          corpus.add(paper)
        } else {
          logger.debug(s"REMOVE from used: Cannot find any free paper containing $method : found = ${usedPapers.getOccurrenceOfMethodOverAllPapers(method)}")
        }
      }
    })
  }

  createCSVFile("usedPapers", usedPapers)
  logger.debug("Used Paper csv created")

  def calcDistance : Int = {
    distribution.map(d => {
      Math.abs(usedPapers.getOccurrenceOfMethodOverAllPapers(d._1) - d._2)
    }).sum
  }

  def createCSVFile(filename: String, papers: PaperContainer): Unit = {
    val wr = CSVWriter.open(new File("./"+filename + pdfsDir.replaceAll("\\Q..\\E", "").replaceAll("/", "_") + ".csv"))
    val sequMeth1 = termLoader.toSeq.map(_.head)
    wr.writeRow("Paper" +: sequMeth1)
    val allPdfs: List[String] = papers.get.flatMap(_._2.map(_.path)).toList.distinct
    allPdfs.foreach(paper => {
      wr.writeRow(paper +: sequMeth1.map(method => papers.getOccurrenceOfMethodForPaper(paper, method)))
    })
    wr.close()
  }

}
