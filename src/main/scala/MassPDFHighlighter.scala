import java.io._

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.joda.time.DateTime
import pdf.PDFManager
import utils.Utils


/**
 * Created by pdeboer on 16/06/15.
 */
object MassPDFHighlighter extends App with LazyLogging {

  val conf = ConfigFactory.load()

  val pdfsDir = conf.getString("highlighter.pdfSourceDir")
  val snippetsDir = conf.getString("highlighter.snippetDir")
  val pathConvert = conf.getString("highlighter.convertCmd")
  val PERMUTATIONS_CSV_FILENAME = conf.getString("highlighter.permutationFilename")

  val startTime = new DateTime().getMillis

  new File(snippetsDir).mkdir()

  Utils.emptyDirRecursively(new File(snippetsDir))

  val isTwoColumn = args.isDefinedAt(0) && args(0).equalsIgnoreCase("2")

  PDFManager(isTwoColumn, pdfsDir, snippetsDir, pathConvert).highlightFiles()

  logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")

}
