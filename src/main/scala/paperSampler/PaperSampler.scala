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

  pdfs.foreach(pdf => {
    val txt = PDFTextExtractor.extractTextAsString(pdf.getAbsolutePath)
    val methods : Map[String, Int] = termLoader.map(terms => {
      val method = terms.head
      val occurrences : Int = terms.map(c => PDFTextExtractor.countAllOccurrences(c, txt)).sum
      method -> occurrences
    }).toMap

    corpus.add(Some(Paper(pdf.getPath, methods)))
  })

  val distribution : Map[String, Int] = termLoader.map(terms => {
    val method = terms.head
    method -> Math.floor(corpus.getOccurrenceOfMethodOverAllPapers(method)*PERCENT / 100.0).toInt
  }).toMap

  createCSVFile("corpus", corpus)

  //corpus.get.foreach(d => logger.debug(d._1 + " ->: papers: " +corpus.countPapersContainingMethod(d._1) + ", total occurrences: " + corpus.getOccurrenceOfMethodOverAllPapers(d._1) ))

  var usedPapers = new PaperContainer()

  var it : Int = 0
  var tmpDistance = calcDistance

  var tmpUsedPapers: PaperContainer = usedPapers.copy
  var tmpCorpus: PaperContainer = corpus.copy

  while(!usedPapers.diff(distribution)){
    it += 1
    val distance = calcDistance
    if(tmpDistance > distance){
      tmpDistance = distance
      tmpUsedPapers = usedPapers.copy
      tmpCorpus = corpus.copy

      createCSVFile("tmpUsedPapers", tmpUsedPapers)
      logger.debug(s"Distance: $tmpDistance")
    }else {
      usedPapers = tmpUsedPapers.copy
      corpus = tmpCorpus.copy
    }

    termLoader.foreach(terms => {
      val method = terms.head
      if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) < distribution.get(method).get){
        usedPapers.add(corpus.removeRandomPaper(method))
      }else if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) > distribution.get(method).get){
        corpus.add(usedPapers.removeRandomPaper(method))
      }
    })
  }

  createCSVFile("usedPapers", usedPapers)


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
