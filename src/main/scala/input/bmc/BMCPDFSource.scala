package input.bmc

import java.io.File

import highlighting.HighlightTermloader
import input.bmc.BMCDAL.DBPaper

/**
 * Created by pdeboer on 17/06/15.
 */
class BMCPDFSource(val basePDFPath: String = "/Users/pdeboer/Documents/phd_local/OpenReviewCrawler/papers") {
	def get(): List[BMCPaper] = {
		val termloader = new HighlightTermloader()
		val papers = termloader.methodsAndSynonyms.par.map(m => {
			termloader.assumptionsAndSynonyms.par.map(a => {
				BMCDAL.getPaperIDsWithTerms(m, a).toSet
			}).toSet.flatten
		}).toSet.flatten

		papers.map(p => BMCPaper(p, new File(basePDFPath + p.filename))).toList
	}

	case class BMCPaper(dBPaper: DBPaper, file: File)
}
