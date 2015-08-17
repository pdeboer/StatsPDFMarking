package input.bmc

import java.io.File

import ch.uzh.ifi.pdeboer.pplib.util.U
import highlighting.HighlightTermloader

/**
 * Created by pdeboer on 17/06/15.
 */
class BMCPDFSource(val basePDFPath: String = "/Users/pdeboer/Documents/phd_local/OpenReviewCrawler/papers") {
	def get(): List[BMCPaper] = {
		val termloader = new HighlightTermloader()
		val papers = termloader.methodsAndSynonyms.par.map(m => {
			termloader.assumptionsAndSynonyms.map(a => {
				U.retry(10)(BMCDAL.getPaperIDsWithTerms(m, a).toSet)
			}).toSet.flatten
		}).toSet.flatten

		papers.map(p => BMCPaper(p, new File(basePDFPath + p.filename))).toList
	}

	case class BMCPaper(dBPaper: DBPaper, file: File)
}
