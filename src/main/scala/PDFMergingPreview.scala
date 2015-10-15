import java.awt.Color
import java.io._

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import highlighting.{MergeMethods, PDFHighlighter, StatMethod}
import input.folder.FolderPDFSource
import org.codehaus.plexus.util.FileUtils
import org.joda.time.DateTime
import pdf._
import utils.Utils


/**
 * Created by pdeboer on 16/06/15.
 */
object PDFMergingPreview extends App with LazyLogging {
	logger.debug("starting merging preview")

	val conf = ConfigFactory.load()

	val INPUT_DIR = conf.getString("highlighter.pdfSourceDir")
	val OUTPUT_DIR = conf.getString("highlighter.snippetDir")
	val CONVERT_CMD = conf.getString("highlighter.convertCmd")
	val PERMUTATIONS_CSV_FILENAME = conf.getString("highlighter.permutationFilename")

	val startTime = new DateTime().getMillis

	new File(OUTPUT_DIR).mkdir()

	Utils.emptyDirRecursively(new File(OUTPUT_DIR))

	val isTwoColumn = args.isDefinedAt(0) && args(0).equalsIgnoreCase("2")

	val mgr = new PDFManager(isTwoColumn, INPUT_DIR, OUTPUT_DIR, CONVERT_CMD)

	val writer = CSVWriter.open(new File("pdfs_and_methods_to_merge.csv"))
	writer.writeRow(List("pdf name", "method name", "method location", "method group"))
	new FolderPDFSource(INPUT_DIR).get().par.foreach(pdfFile => {
		val methodsToMerge = mgr.terms.methods.par.map(method => {
			val delta = mgr.terms.getDeltaForMethod(method)
			val methodAndSynonyms = mgr.terms.getMethodAndSynonymsFromMethodName(method).get
			val availableMethods: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.methodName) ::: methodAndSynonyms.synonyms))

			try {
				val permuter = new PDFPermuter(pdfFile.getAbsolutePath)
				val maxLengthPDF = PDFTextExtractor.extract(pdfFile.getAbsolutePath).map(_.length).sum

				val methodList = permuter.findAllMethodsInPaper(availableMethods).sortBy(m => mgr.calculateIndexPositionOfMethod(permuter, m))


				val methodsToMerge = mgr.createStatMethodList(PaperHighlightManager(delta, permuter, methodList, maxLengthPDF))

				MethodInPaper(pdfFile.getName, method, methodAndSynonyms, permuter, methodsToMerge)
			} catch {
				case e: Exception => {
					Utils.copyIntoErrorFolder("errors_whilePermuting", pdfFile, e)
					null
				}
			}
		}).filter(_ != null)

		methodsToMerge.zipWithIndex.foreach(mi => {
			mi._1.methodsToMerge.zipWithIndex.foreach(sm => {
				sm._1.instructions.foreach(i => {
					writer.writeRow(List(pdfFile.getName, i.highlightString, i.startSearchStringIndex, mi._2 * 1000 + sm._2))
				})
			})
		})



		val highlightingInstructions = methodsToMerge.map(methodInPaper => {
			if (methodInPaper.methodsToMerge.nonEmpty) {
				val (mergedMethods: List[StatMethod], assumptionsList: List[PDFHighlightInstruction]) = MergeMethods.mergeMethods(methodInPaper)
				Some(mergedMethods.zipWithIndex.map(groupedMethods => HighlightInstruction(groupedMethods._2, groupedMethods._1.instructions, assumptionsList)))
			} else None
		}).filter(_.isDefined).flatMap(_.get).toList

		if (highlightingInstructions.nonEmpty) {
			val filename: String = "output/" + pdfFile.getName
			FileUtils.copyFile(pdfFile, new File(filename))
			val byteArray = new PDFHighlighter(filename, highlightingInstructions.flatMap(_.methodsList)).highlight()

			Some(new BufferedOutputStream(new FileOutputStream(filename))).foreach(s => {
				s.write(byteArray._2)
				s.close()
			})
		}
		logger.debug(s"processed $pdfFile")
	})
	writer.close()


	logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")

}
