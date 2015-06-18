import java.awt.Color
import java.io.{BufferedOutputStream, File, FileOutputStream}

import highlighting.{HighlightTermloader, PDFHighlight}
import input.bmc.BMCPDFSource

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App {
	val outputDir = "output/"

	new File(outputDir).mkdirs()

	new BMCPDFSource().get().par.foreach(f => {
		val h = new PDFHighlight(f.getAbsolutePath)

		val terms = new HighlightTermloader
		val highlighted = h.highlight(Map(Color.yellow -> terms.methodsAndSynonyms, Color.green -> terms.assumptionsAndSynonms))

		Some(new BufferedOutputStream(new FileOutputStream(outputDir + f.getName))).foreach(s => {
			s.write(highlighted)
			s.close()
		})

		println(s"processed $f")
	})
}
