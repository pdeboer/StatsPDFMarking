package input.bmc

import java.io.File

import highlighting.HighlightTermloader

/**
 * Created by pdeboer on 17/06/15.
 */
class BMCPDFSource(val basePDFPath: String = "/Users/pdeboer/Documents/phd_local/OpenReviewCrawler/papers") {
	def get(): Iterable[File] = {
		val termloader = new HighlightTermloader()
		val papers = termloader.methodsAndSynonyms.map(m => {
			termloader.assumptionsAndSynonms.map(a => {
				DAL.getPaperIDsWithTerms(m, a).toSet
			}).toSet.flatten
		}).toSet.flatten

		papers.map(p => new File(basePDFPath+p.filename))
	}
}
