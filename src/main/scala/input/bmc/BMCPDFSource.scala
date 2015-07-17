package input.bmc

import java.io.File

import highlighting.HighlightTermloader
import input.PDFSource

/**
 * Created by pdeboer on 17/06/15.
 */
class BMCPDFSource(val basePDFPath: String = "/Users/pdeboer/Documents/phd_local/OpenReviewCrawler/papers") extends PDFSource {
	def get(): Iterable[File] = {
		val termloader = new HighlightTermloader()
		val papers = termloader.methodsAndSynonyms.par.map(m => {
			termloader.assumptionsAndSynonyms.par.map(a => {
				BMCDAL.getPaperIDsWithTerms(m, a).toSet
			}).toSet.flatten
		}).toSet.flatten

		papers.map(p => new File(basePDFPath+p.filename)).toList
	}
}
