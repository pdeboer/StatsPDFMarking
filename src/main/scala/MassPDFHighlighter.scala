import java.awt.Color
import java.io.{BufferedOutputStream, File, FileOutputStream, FilenameFilter}

import com.typesafe.scalalogging.LazyLogging
import highlighting.{HighlightTermloader, PDFPermuter}
import input.folder.FolderPDFSource
import org.joda.time.DateTime

import scala.sys.process._

/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging {

	val pdfsDir = "../pdfs2/"
	val outputDir = "../output/"

	val pathConvert = "/opt/local/bin/convert"
	val pathGS = "/opt/local/bin/gs"

	val startTime = new DateTime().getMillis

	createOrEmptyOutputDir

	highlightPDFFile

	logger.debug("Starting conversion PDF2PNG...")

	new File(outputDir).listFiles(new FilenameFilter {
		override def accept(dir: File, name: String): Boolean = dir.isDirectory
	}).par.foreach(methodDirectory => {
		methodDirectory.listFiles(new FilenameFilter {
			override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
		}).par.foreach(pdfFile => {
			convertPDFtoPNG(pdfFile)
		})
	})

	logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")


	def createOrEmptyOutputDir = {
		new File(outputDir).mkdir()
		new File(outputDir).listFiles().foreach(f => {
			if (f.isDirectory) {
				f.listFiles().foreach(img => {
					if (img.isDirectory) {
						img.listFiles().foreach(ff => ff.delete())
					}
					img.delete()
				})
			}
			f.delete()
		})
	}

	def highlightPDFFile = {
		new FolderPDFSource(pdfsDir).get().par.foreach(f => {
			highlightFile(f)
			logger.info(s"processed $f")

		})
	}

	def highlightFile(f: File) = {
		val terms = new HighlightTermloader

		terms.termNames.foreach(method => {

			println(s"Highlighting method $method")

			val methodAndSynonyms = terms.getMethodAndSynonymsFromMethodName(method).get
			var assumptionsAndSynonyms: List[String] = List.empty[String]

			methodAndSynonyms.assumptions.foreach(assumption => {
				assumptionsAndSynonyms = assumptionsAndSynonyms ::: List[String](assumption.name) ::: assumption.synonym
			})

			val colorToStrings: Map[Color, List[String]] = Map(Color.yellow -> (List[String](methodAndSynonyms.name) ::: methodAndSynonyms.synonyms),
				Color.green -> assumptionsAndSynonyms)

			new PDFPermuter(f.getAbsolutePath).permuteForEachCombinationOf(colorToStrings).zipWithIndex.par.foreach(
				highlighter => {
					logger.debug(s"${highlighter._2}_${f.getName}: highlighting combination of ${highlighter._1.instructions}")

					val methodName = terms.getMethodFromSynonymOrMethod(highlighter._1.instructions.head.highlightString).get.name.replaceAll(" ", "_")
					if (methodName != "ANOVA")
						println(methodName)
					new File(outputDir + "/" + methodName).mkdirs()
					Some(new BufferedOutputStream(new FileOutputStream(outputDir + "/" + methodName + "/" + highlighter._2 + "_" + f.getName))).foreach(s => {
						s.write(highlighter._1.highlight())
						s.close()
					})
				})

		})


	}

	def convertPDFtoPNG(pdfFile: File) = {
		val pathPDFFile = pdfFile.getPath
		val pathConvertedPNGFile: String = createSubDirForPNGs(pdfFile)

		val convertCommandWithParams = "nice -n 5 " + pathConvert + " -density 200 -append "

		try {

			logger.error((convertCommandWithParams + pathPDFFile.replaceAll(" ", "\\ ") + " " + pathConvertedPNGFile.replaceAll(" ", "\\ ")).lineStream_!.mkString("\n"))

			logger.debug(s"File: ${pdfFile.getName} successfully converted to PNG")
		} catch {
			case e: Exception => logger.error(s"Cannot convert ${pdfFile.getName} to PNG.", e)
		}
	}

	def createSubDirForPNGs(pdfFile: File): String = {
		val subDirPNGFiles = removePDFExtension(pdfFile.getPath)
		val pathConvertedPNGFile = subDirPNGFiles + "/" + createPNGFileName(pdfFile.getName)

		new File(subDirPNGFiles).mkdirs()
		pathConvertedPNGFile
	}

	def createPNGFileName(filename: String): String = {
		filename + ".png"
	}

	def removePDFExtension(fileName: String): String = {
		fileName.substring(0, fileName.length - 4)
	}
}
