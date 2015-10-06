package paperSampler

import java.io.File

import scala.io.Source

/**
 * Created by pdeboer on 06/10/15.
 */
object PaperTextSearcher extends App {
	val allText = new File("/Users/pdeboer/Downloads/pdfname/").listFiles().map(f => Source.fromFile(f).getLines().toSet).mkString("")
	println("test".r.findAllIn(allText).size)
}
