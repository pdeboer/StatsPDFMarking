package paperSampler

import java.io.File

import com.github.tototoshi.csv.CSVWriter

/**
 * Created by mattia on 29.09.15.
 */
object SamplerWriter {

	def createCSVFile(filename: String, papers: PaperContainer, detailFormat: Boolean, termLoader: List[List[String]], journals: List[String]): Unit = {
		val sequMeth1 = termLoader.toSeq.map(_.head)
		if (detailFormat) {
			val wr = CSVWriter.open(new File("./" + filename + ".csv"))
			wr.writeRow("Paper" +: sequMeth1)
			val allPdfs: List[String] = papers.get.flatMap(_._2.map(_.path)).toList.distinct
			allPdfs.foreach(paper => {
				wr.writeRow(paper +: sequMeth1.map(method => papers.countMethodOccurrencesInPaper(paper, method)))
			})
			wr.close()
		} else {
			val wr = CSVWriter.open(new File("./" + filename + ".csv"))
			wr.writeRow("Method" +: journals)
			sequMeth1.foreach(method => {
				wr.writeRow(method +: journals.map(journal => papers.countMethodOccurrences(method, journal)))
			})

			wr.close()
		}

	}

}
