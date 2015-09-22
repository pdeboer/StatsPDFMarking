package paperSampler

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.scalalogging.LazyLogging
import highlighting.HighlightTermloader
import input.folder.FolderPDFSource
import pdf.PDFTextExtractor

/**
 * Created by mattia on 21.09.15.
 */
object PaperSampler extends App with LazyLogging {

  val pdfsDir = args(0)
  logger.info("PDFs DIR: " + pdfsDir)
  val PERCENT = args(1).toDouble

  val pdfs = new FolderPDFSource(pdfsDir).get().toList

  // corpus contains the occurrences of every methods for each paper
  val corpus = new PaperContainer()

  val termLoader = new HighlightTermloader()

  val availableMethods : List[String] = termLoader.methods

  pdfs.par.foreach(pdf => {
    val txt = PDFTextExtractor.extract(pdf.getAbsolutePath)
    val methods : Map[String, Int] = availableMethods.map(method => {
      val synonyms : List[String] = termLoader.getMethodAndSynonymsFromMethodName(method).get.synonyms
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

  val writer = CSVWriter.open(new File("./corpus"+pdfsDir.replaceAll("..","").replaceAll("/","_")+".csv"))
  val sequMeth = availableMethods.toSeq
  writer.writeRow("Paper" +: sequMeth)
  pdfs.foreach(pdf => {
    writer.writeRow(pdf.getPath +: sequMeth.map(method => corpus.getOccurrenceOfMethodForPaper(pdf.getPath, method)))
  })
  writer.close()

  val usedPapers = new PaperContainer

  while(!usedPapers.diff(distribution)){
    availableMethods.foreach(method => {
      if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) < distribution.get(method).get){
        usedPapers.add(corpus.removeRandomPaper(method))
      }else if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) > distribution.get(method).get){
        corpus.add(usedPapers.removeRandomPaper(method))
      }
    })
  }

  val writer1 = CSVWriter.open(new File("./usedPapers"+pdfsDir.replaceAll("..","").replaceAll("/","_")+".csv"))
  val sequMeth1 = availableMethods.toSeq
  writer1.writeRow("Paper" +: sequMeth1)
  val allPdfs : List[String] = usedPapers.get.flatMap(_._2.map(_.path)).toList.distinct
  allPdfs.foreach(paper => {
    writer1.writeRow(paper +: sequMeth1.map(method => usedPapers.getOccurrenceOfMethodForPaper(paper, method)))
  })

  writer1.close()

  distribution.foreach(d => logger.debug(d._1 + " ->: " + corpus.getOccurrenceOfMethodOverAllPapers(d._1) + " * "+ PERCENT+"%  => " + d._2 + " == " + usedPapers.getOccurrenceOfMethodOverAllPapers(d._1)))


}
