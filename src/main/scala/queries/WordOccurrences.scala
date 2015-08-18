package queries

import java.io.{File, FileFilter}

import highlighting.{HighlightTermloader, PDFTextExtractor}

/**
 * Created by mattia on 03.08.15.
 */
object WordOccurrences extends App {

  val pdfSourceFolder = "../pdfs2/"
  val methodsAndSynonyms = new HighlightTermloader().methodsAndSynonyms

  var skipped = 0

  var words = collection.mutable.Map.empty[Int, Int]
  var methods2matches = collection.mutable.Map.empty[String, collection.mutable.Map[Int, Int]]

  new File(pdfSourceFolder).listFiles(new FileFilter {
    override def accept(pathname: File): Boolean = pathname.getName.endsWith(".pdf")
  }).foreach(pdfFile => {

    methodsAndSynonyms.foreach(t => {

        try {
          val txt = PDFTextExtractor.extract(pdfFile.getPath).mkString("")

          val allMatches = t.r.findAllMatchIn(txt).map(_.start).foreach(position => {

            if(words.get(position).isDefined) {
              words.update(position, words.get(position).get+1)
            } else {
              words +=(position -> 1)
            }

            if(methods2matches.get(t).isDefined){
              if(methods2matches.get(t).get.get(position).isDefined){
                methods2matches.get(t).get.update(position, methods2matches.get(t).get.get(position).get+1)
              } else {
                methods2matches.get(t).get += (position -> 1)
              }
            }
          })

        } catch {
          case e: Throwable => {
            skipped += 1
          }
        }
        if(methods2matches.get(t).isEmpty){
          methods2matches += (t -> collection.mutable.Map.empty[Int, Int])
        }

    })
  })

  println(words.toSeq.sortBy(_._1).mkString("\n"))
  println()
  println()

  println(methods2matches.toSeq.sortBy(_._1).mkString("\n\n"))
  println(s"Skipped: $skipped")


}
