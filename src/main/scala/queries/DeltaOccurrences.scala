package queries

import java.io.{File, FileFilter}

import highlighting.HighlightTermloader
import pdf.PDFTextExtractor

/**
 * Created by mattia on 03.08.15.
 */
object DeltaOccurrences extends App{

  val pdfSourceFolder = "../pdfs2/"
  val methodsAndSynonyms = new HighlightTermloader().methodsAndSynonyms

  var matches = collection.mutable.Map.empty[Int, Match]

  new File(pdfSourceFolder).listFiles(new FileFilter {
    override def accept(pathname: File): Boolean = pathname.getName.endsWith(".pdf")
  }).foreach(pdfFile => {

    val txt = PDFTextExtractor.extract(pdfFile.getPath)
    var allMatches = List.empty[Int]

    methodsAndSynonyms.foreach(t1 => {
      t1.r.findAllMatchIn(txt.mkString("")).foreach(m => {
        allMatches ::= m.start
      })
    })

    for(i <- 0 to allMatches.length-1){
      for(j <- i to allMatches.length-1){
        val delta = Math.abs(allMatches(i) - allMatches(j))

        if(matches.get(delta).isDefined) {
          var fileList = matches.get(delta).get.filenames

          if(!fileList.contains(pdfFile.getName)){
            fileList ::= pdfFile.getName
          }

          matches.update(delta, Match(matches.get(delta).get.nOfOccurrences+1, fileList))
        } else {
          matches += (delta -> Match(filenames = List[String](pdfFile.getName)))
        }

      }
    }

  })

  matches.toSeq.sortBy(_._1).foreach(m => {
    println(m._1+","+m._2.nOfOccurrences+","+m._2.filenames.mkString(";"))
  })

}
