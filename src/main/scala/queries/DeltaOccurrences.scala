package queries

import java.io.{File, FileFilter}

import highlighting.{HighlightTermloader, PDFTextExtractor}

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

    println(s"Working file: ${pdfFile.getName}")

    val txt = PDFTextExtractor.extract(pdfFile.getPath)
    var allMatches = List.empty[Int]

    methodsAndSynonyms.foreach(t1 => {
      t1.r.findAllMatchIn(txt).foreach(m => {
        allMatches ::= m.start
      })
    })

    (allMatches, allMatches drop 1).zipped.map(_ - _).foreach( d => {
      val delta = Math.abs(d)

      if(matches.get(delta).isDefined) {
        var fileList = matches.get(delta).get.filenames

        if(!fileList.contains(pdfFile.getName)){
          fileList ::= pdfFile.getName
        }

        matches.update(delta, Match(matches.get(delta).get.nOfOccurrences+1, fileList))
      } else {
        matches += (delta -> Match(filenames = List[String](pdfFile.getName)))
      }

    })


  })

  println(matches.toSeq.sortBy(_._1).mkString("\n"))

}
