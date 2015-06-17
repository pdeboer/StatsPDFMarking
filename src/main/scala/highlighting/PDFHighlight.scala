package highlighting

import java.io.{ByteArrayOutputStream, FileInputStream}

import com.typesafe.scalalogging.LazyLogging
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument

/**
 * Created by pdeboer on 16/06/15.
 */
class PDFHighlight(val pdfPath:String) extends LazyLogging {

	/**
	 * taken from Mattia's code
	 * Highlight all the words contained in the contentCsv variable
	 * @param wordsToHighlight a List of strings containing all the words/phrases to highlight in the PDF
	 */
	def highlight(wordsToHighlight: List[String]) : Array[Byte] = {
		val file = "./public"+pdfPath
		val parser: PDFParser = new PDFParser(new FileInputStream(file))
		parser.parse()
		val pdDoc: PDDocument = new PDDocument(parser.getDocument)

		val pdfHighlight: TextHighlight = new TextHighlight("UTF-8")
		// depends on what you want to match, but this creates a long string without newlines
		pdfHighlight.setLineSeparator(" ")
		pdfHighlight.initialize(pdDoc)

		for(textRegEx <- wordsToHighlight) {
			logger.debug("Highlighting: " + textRegEx)
			pdfHighlight.highlightDefault(textRegEx)
		}

		val byteArrayOutputStream = new ByteArrayOutputStream()
		try {
			if (pdDoc != null) {
				pdDoc.save(byteArrayOutputStream)
				pdDoc.close()
			}
			if (parser.getDocument != null) {
				parser.getDocument.close
			}
		}
		catch {
			case e: Exception => {
				e.printStackTrace
			}
		}
		byteArrayOutputStream.toByteArray()
	}
}
