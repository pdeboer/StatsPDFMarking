import java.awt.Color
import java.io.{BufferedOutputStream, File, FileOutputStream}

import highlighting.{HighlightTermloader, PDFPermuter}
import input.folder.FolderPDFSource

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App {
	val outputDir = "output/"

	new File(outputDir).mkdirs()
	new File(outputDir).listFiles().foreach(f => f.delete())


	new FolderPDFSource("pdfs").get().par.foreach(f => {
		highlightFile(f)

		println(s"processed $f")
	})

	def highlightFile(f: File): Unit = {
		val terms = new HighlightTermloader
		val colorToStrings: Map[Color, List[String]] = Map(Color.yellow -> terms.methodsAndSynonyms, Color.green -> terms.assumptionsAndSynonyms)


		new PDFPermuter(f.getAbsolutePath).permuteForEachCombinationOf(colorToStrings).zipWithIndex.foreach(highlighter => {
			print("highlighting combination of " + highlighter._1.instructions)

			Some(new BufferedOutputStream(new FileOutputStream(outputDir + highlighter._2 + "_" + f.getName))).foreach(s => {
				s.write(highlighter._1.highlight())
				s.close()
			})
		})

	}
}
