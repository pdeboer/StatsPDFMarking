import java.io.{File, FileInputStream, FileOutputStream}

import input.bmc.BMCPDFSource

/**
 * Created by pdeboer on 10/08/15.
 */
object PDFCopy extends App {
	/*
	//load from method list
	val occurrences = Source.fromFile("methodlist.csv").getLines().toList.par.flatMap(l => {
		val terms = l.split(",").map(_.trim())
		val papersWithTermVariations = terms.flatMap(t => {
			val targetTerms = if (t.length < 7) MethodOccurrences.addWordBoundaries(t) else List(t)
			targetTerms.flatMap(tt => BMCDAL.getPapersContainingTerm(tt).map(o => PaperOccurrence(o)(List(tt))))
		})
		papersWithTermVariations
	}).toSet
	*/

	val outputPath = "/Users/pdeboer/Downloads/bmc/"
	new BMCPDFSource().get().foreach(p => {
		println(s"copying ${p.file.getName}")
		val outfile = new File(outputPath + p.dBPaper.year + "_" + p.file.getName)
		outfile.createNewFile()
		new FileOutputStream(outfile) getChannel() transferFrom(
			new FileInputStream(p.file) getChannel, 0, Long.MaxValue)
	})
}
