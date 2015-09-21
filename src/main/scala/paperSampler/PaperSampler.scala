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

  val PERCENT = 50
  val pdfsDir = "../ElsevierAccess/download/"

  val pdfs = new FolderPDFSource(pdfsDir).get().toList

  // corpus contains the occurrences of every methods for each paper
  val corpus = new PaperContainer()

  val termLoader = new HighlightTermloader()

  val methods : List[String] = termLoader.methods

  pdfs.par.foreach(pdf => {
    val txt = PDFTextExtractor.extract(pdf.getAbsolutePath)
    val paper : Map[String, Int] = methods.map(method => {
      val synonyms : List[String] = termLoader.getMethodAndSynonymsFromMethodName(method).get.synonyms
      val occurrencesAllSynonyms = synonyms.map(s => PDFTextExtractor.countAllOccurrences(s, txt)).sum
      val occurrencesMethod = PDFTextExtractor.countAllOccurrences(method, txt)
      method -> (occurrencesAllSynonyms+occurrencesMethod)
    }).toMap
    corpus.add(Some(Paper(pdf.getPath, paper)))
  })

  val distribution : Map[String, Int] = methods.map(method => {
    method -> Math.floorDiv(corpus.getOccurrenceOfMethodOverAllPapers(method)*PERCENT, 100)
  }).toMap

  distribution.foreach(d => println(d._1 + " ->: " + corpus.getOccurrenceOfMethodOverAllPapers(d._1) + " * "+ PERCENT+"%  => " + d._2))

  val writer = CSVWriter.open(new File("./corpus.csv"))
  val sequMeth = methods.toSeq
  writer.writeRow("Paper" +: sequMeth)
  pdfs.foreach(pdf => {
    writer.writeRow(pdf.getPath +: sequMeth.map(method => corpus.getOccurrenceOfMethodForPaper(pdf, method)))
  })
  writer.close()

  val usedPapers = new PaperContainer

  while(usedPapers.diff(distribution)){
    methods.foreach(method => {
      if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) < distribution.get(method).get){
        usedPapers.add(corpus.removeRandomPaper(method))
      }else if(usedPapers.getOccurrenceOfMethodOverAllPapers(method) > distribution.get(method).get){
        corpus.add(usedPapers.removeRandomPaper(method))
      }
    })
  }

  println("USED PAPERS:")
  println(usedPapers.toString())

  val writer1 = CSVWriter.open(new File("./usedPapers.csv"))
  val sequMeth1 = methods.toSeq
  writer1.writeRow("Paper" +: sequMeth1)
  usedPapers.get.foreach(paper => {
    writer1.writeRow(paper._1 +: sequMeth1.map(method => usedPapers.getOccurrenceOfMethodForPaper(new File(paper._1), method)))
  })

  writer1.close()

}
