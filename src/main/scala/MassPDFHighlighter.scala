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
  logger.debug("starting highlighting")

  val conf = ConfigFactory.load()

  val INPUT_DIR = conf.getString("highlighter.pdfSourceDir")
  val OUTPUT_DIR = conf.getString("highlighter.snippetDir")
  val CONVERT_CMD = conf.getString("highlighter.convertCmd")
  val PERMUTATIONS_CSV_FILENAME = conf.getString("highlighter.permutationFilename")

  val startTime = new DateTime().getMillis

  new File(OUTPUT_DIR).mkdir()

  Utils.emptyDirRecursively(new File(OUTPUT_DIR))

  val isTwoColumn = args.isDefinedAt(0) && args(0).equalsIgnoreCase("2")
  logger.debug(s"are we in two-column mode? $isTwoColumn")

  new PDFManager(isTwoColumn, INPUT_DIR, OUTPUT_DIR, CONVERT_CMD).highlightFiles()

  logger.debug(s"Process finished in ${(new DateTime().getMillis - startTime) / 1000} seconds")

}
