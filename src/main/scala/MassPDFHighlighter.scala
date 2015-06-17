import java.io.{FileOutputStream, BufferedOutputStream, File}

import dal.DAL
import highlighting.{HighlightTermloader, PDFHighlight}

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App {
	val outputDir = "output/"

	val p = DAL.getPaperIDsWithTerms("anova", "normal")

	def highlight: Unit = {
		val terms = HighlightTermloader.load()
		new File(outputDir).mkdirs()

		new File("pdfs/").listFiles().foreach(f => {
			val h = new PDFHighlight(f.getAbsolutePath)
			val termNames = terms.map(_.name).toList
			val termSynonyms = terms.flatMap(t => t.synonyms).toList
			val termAssumptions = terms.flatMap(t => t.assumptions.map(a => a.name)).toList
			val termAssumptionSynonyms = terms.flatMap(t => t.assumptions.flatMap(a => a.synonym)).toList

			val highlighted = h.highlight(termNames ::: termSynonyms ::: termAssumptions ::: termAssumptionSynonyms)

			Some(new BufferedOutputStream(new FileOutputStream(outputDir + f.getName))).foreach(s => {
				s.write(highlighted); s.close()
			})

			println(s"processed $f")
		})
	}
}
