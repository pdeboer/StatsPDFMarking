package highlighting

import java.io.{ByteArrayOutputStream, FileInputStream}

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument
import pdf.PDFHighlightInstruction
import utils.Utils

/**
 * Created by pdeboer on 16/06/15.
 */

class PDFHighlighter(val pdfPath: String, val instructions: List[PDFHighlightInstruction]) extends LazyLogging {
	/**
	 * taken from Mattia's code and adapted
	 */
	def highlight(): (HighlightPage, Array[Byte]) = {
		try {
			val pdfFile = pdfPath
			val parser: PDFParser = new PDFParser(new FileInputStream(pdfFile))
			parser.parse()
			val pdDoc: PDDocument = new PDDocument(parser.getDocument)

			val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
			pdfHighlight.setLineSeparator(" ")
			pdfHighlight.initialize(pdDoc)

			val processedPages: List[Int] = instructions.map(i => {
				Utils.escapeSearchString(i.searchString).foreach(searchString => {
					Utils.escapeSearchString(i.highlightString).foreach(highlightString => {
						pdfHighlight.highlight(searchString.r.pattern, highlightString.r.pattern, i.color, i.pageNr)
					})
				})
				i.pageNr - 1
			})

			val byteArrayOutputStream = new ByteArrayOutputStream()

			if (pdDoc != null) {
				pdDoc.save(byteArrayOutputStream)
				pdDoc.close()
			}
			if (parser.getDocument != null) {
				parser.getDocument.close()
			}

			(HighlightPage(processedPages.min, processedPages.max), byteArrayOutputStream.toByteArray)
		} catch {
			case e: Exception => {
				logger.error(s"Cannot store highlighted version of pdf: $pdfPath.", e)
				(HighlightPage(-1, -1), Array.empty[Byte])
			}
		}
	}
}

case class HighlightPage(start: Int, end: Int)