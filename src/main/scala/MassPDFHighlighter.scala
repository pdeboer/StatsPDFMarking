import java.io.File

import input.PDFSource
import input.bmc.BMCPDFSource

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App {
	val outputDir = "output/"

	new File(outputDir).mkdirs()
	new File(outputDir).listFiles().foreach(f => f.delete())

	private val source: PDFSource = new BMCPDFSource() // new FolderPDFSource("pdfs")


	source.get().foreach(f => {
		import java.io.{FileInputStream, FileOutputStream}
		new FileOutputStream("/Users/pdeboer/Downloads/papers/" + f.getName) getChannel() transferFrom(
			new FileInputStream(f.getAbsolutePath) getChannel, 0, Long.MaxValue)

		println("copied " + f.getAbsolutePath)
	})

	/*
	source.get().par.foreach(f => {
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

	}*/
}
