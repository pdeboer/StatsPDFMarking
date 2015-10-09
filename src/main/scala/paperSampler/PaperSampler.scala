package paperSampler

import java.io.{File, FilenameFilter}

import ch.uzh.ifi.pdeboer.pplib.process.entities.FileProcessMemoizer
import com.typesafe.scalalogging.LazyLogging
import pdf.PDFTextExtractor

import scala.io.Source

/**
 * Created by mattia on 21.09.15.
 */
object PaperSampler extends App with LazyLogging {

	val allJournalsDir = args(0)
	val PERCENT = args(1).toDouble
	val distributionOverAllJournals = args(2).toBoolean

	val mem = new FileProcessMemoizer("papersampler")

	logger.debug(s"Journals DIR: $allJournalsDir")
	val journalsToPdfs: Map[String, List[File]] = getJournalToPdfsMap

	val journals = mem.mem("journals")(journalsToPdfs.keys.toList)

	val termLoader: List[List[String]] = Source.fromFile("methodlist_full.csv", "UTF-8").getLines().map(l => {
		l.split(",").map(_.trim()).toList
	}).toList

	val corpus = mem.mem("corpus")(createCorpus())

	private val globalCorpusName: String = mem.mem("globalcorpusname")(allJournalsDir.replaceAll("\\Q..\\E", "").replaceAll("/", "_"))
	SamplerWriter.createCSVFile("corpus" + globalCorpusName, corpus, false, termLoader, journals)
	//SamplerWriter.createCSVFile("plainCorpus"+ allJournalsDir.replaceAll("\\Q..\\E", "").replaceAll("/", "_"), plainCorpus, false, termLoader, journals)
	logger.debug("Corpus csv created")


	var distribution: Map[String, Int] = calculateDistribution(corpus)

	journals.foreach(journal => {
		val journalCorpus = createJournalCorpus(corpus, journal)
		SamplerWriter.createCSVFile(s"corpus_$journal", journalCorpus, true, termLoader, journals)


		if (!distributionOverAllJournals) {
			distribution = calculateDistribution(journalCorpus)
		}

		logger.debug(s"Start algorithm for journal $journal...")
		val usedPapers = SamplerAlgorithm.findByRandomness(journalCorpus, journal) //SamplerAlgorithm.findBestPath(journalCorpus)

		SamplerWriter.createCSVFile(s"usedPapers_$journal", usedPapers, true, termLoader, journals)
		logger.debug("Used Paper csv created")
	})

	def createCorpus() = {
		val container = new PaperContainer

		journalsToPdfs.par.foreach(journal => {
			journal._2.par.foreach(pdf => {

				val pdfTxt = try {
					PDFTextExtractor.extract(pdf.getAbsolutePath)
				}
				catch {
					case e: Throwable => println(s"couldn't read $pdf"); e.printStackTrace(); List("")
				}
				//val pdfPlainTxt = PDFTextExtractor.extractTextAsString(pdf.getAbsolutePath)

				val methods: Map[String, Int] = termLoader.map(terms => {
					val method = terms.head
					val occurrenceMap = terms.map(term => term -> PDFTextExtractor.countAllOccurrences(term, pdfTxt.mkString(""))).toMap
					method -> occurrenceMap.values.sum
				}).toMap

				container.add(Some(Paper(pdf.getPath, journal._1, methods)))
			})
		})
		container
	}

	def getJournalToPdfsMap: Map[String, List[File]] = {
		new File(allJournalsDir).listFiles(new FilenameFilter {
			override def accept(dir: File, name: String): Boolean = new File(dir, name).isDirectory
		}).map(journalDir => journalDir.getName -> journalDir.listFiles(new FilenameFilter {
			override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
		}).toList).toMap
	}

	def calculateDistribution(corp: PaperContainer): Map[String, Int] = {
		if (distributionOverAllJournals) {
			val dist = termLoader.map(terms => {
				val method = terms.head
				method -> Math.floor(corp.countMethodOccurrences(method) * PERCENT / 100.0).toInt
			}).toMap
			logger.debug("Distribution defined")
			dist
		} else {
			Map.empty[String, Int]
		}
	}

	def createJournalCorpus(corpus: PaperContainer, journal: String): PaperContainer = {
		val papersOfThisJournal = corpus.get.flatMap(_._2.filter(_.journal.equalsIgnoreCase(journal)))
		val c = new PaperContainer()
		papersOfThisJournal.foreach(p => c.add(Some(p)))
		c
	}

	def calcDistance(papers: PaperContainer): Double = {
		Math.sqrt(distribution.map(d => {
			Math.pow(papers.countMethodOccurrences(d._1) - d._2, 2.0)
		}).sum)
	}

}
