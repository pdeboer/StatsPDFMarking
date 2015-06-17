import java.io.{FileOutputStream, BufferedOutputStream, File}

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

		val highlighted = h.highlight(new HighlightTermloader().allTerms)

		Some(new BufferedOutputStream(new FileOutputStream(outputDir + f.getName))).foreach(s => {
			s.write(highlighted)
			s.close()
		})

		println(s"processed $f")
	})
}
