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
  logger.info("PDFs DIR: " + pdfsDir)
  val PERCENT = args(1).toDouble

  val pdfs = new FolderPDFSource(pdfsDir).get().toList

  // corpus contains the occurrences of every methods for each paper
  var corpus = new PaperContainer()

  val termLoader : Map[String, List[String]] = Source.fromFile("methodlist.csv").getLines().map(l => {
    val cols = l.split(",")
    (cols(0), cols.drop(1).toList)
  }).toMap

  val availableMethods : List[String] = termLoader.keys.toList

  pdfs.par.foreach(pdf => {
    val txt = PDFTextExtractor.extract(pdf.getAbsolutePath)
    val methods : Map[String, Int] = availableMethods.map(method => {
      val synonyms : List[String] = termLoader.getOrElse(method, List.empty)
      val occurrencesAllSynonyms = synonyms.map(s => PDFTextExtractor.countAllOccurrences(s, txt)).sum
      val occurrencesMethod = PDFTextExtractor.countAllOccurrences(method, txt)
      if(occurrencesAllSynonyms+occurrencesMethod > 0){
        method -> (occurrencesAllSynonyms+occurrencesMethod)
      }
    }).collect({case pair: (String, Int) => pair}).toMap
    corpus.add(Some(Paper(pdf.getPath, methods)))
  })

  val distribution : Map[String, Int] = availableMethods.map(method => {
    method -> Math.floor(corpus.getOccurrenceOfMethodOverAllPapers(method)*PERCENT / 100.0).toInt
  }).toMap

  createCSVFile("corpus", corpus)

  var usedPapers = new PaperContainer

  var it : Int = 0
  var tmpDistance = calcDistance

  var tmpUsedPapers: PaperContainer = usedPapers
  var tmpCorpus: PaperContainer = corpus

  while(!usedPapers.diff(distribution)){
    it += 1
    val distance = calcDistance
    if(tmpDistance > distance){
      tmpDistance = distance
      tmpUsedPapers = usedPapers.copy
      tmpCorpus = corpus.copy

      createCSVFile("tmpUsedPapers", tmpUsedPapers)
      logger.info(s"Distance: $tmpDistance")
    }else {
      usedPapers = tmpUsedPapers
      corpus = tmpCorpus
    }

    availableMethods.foreach(method => {
      if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) < distribution.get(method).get){
        usedPapers.add(corpus.removeRandomPaper(method))
      }else if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) > distribution.get(method).get){
        corpus.add(usedPapers.removeRandomPaper(method))
      }
    })
  }

  createCSVFile("usedPapers", usedPapers)

  distribution.foreach(d => logger.debug(d._1 + " ->: " + corpus.getOccurrenceOfMethodOverAllPapers(d._1) + " * "+ PERCENT+"%  => " + d._2 + " == " + usedPapers.getOccurrenceOfMethodOverAllPapers(d._1)))

  def calcDistance : Int = {
    distribution.map(d => {
      Math.abs(usedPapers.getOccurrenceOfMethodOverAllPapers(d._1) - d._2)
    }).sum
  }

  def createCSVFile(filename: String, papers: PaperContainer): Unit = {
    val wr = CSVWriter.open(new File("./"+filename + pdfsDir.replaceAll("\\Q..\\E", "").replaceAll("/", "_") + ".csv"))
    val sequMeth1 = availableMethods.toSeq
    wr.writeRow("Paper" +: sequMeth1)
    val allPdfs: List[String] = papers.get.flatMap(_._2.map(_.path)).toList.distinct
    allPdfs.foreach(paper => {
      wr.writeRow(paper +: sequMeth1.map(method => papers.getOccurrenceOfMethodForPaper(paper, method)))
    })
    wr.close()
  }

}
