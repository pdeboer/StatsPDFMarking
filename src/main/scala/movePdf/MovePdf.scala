package movePdf

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.typesafe.scalalogging.LazyLogging
import org.codehaus.plexus.util.FileUtils

/**
 * Created by mattia on 23.09.15.
 */
object MovePdf extends App with LazyLogging {

  val usedPapersPath = args(0)
  val reader = CSVReader.open(new File(usedPapersPath))

  val lines = reader.all()

  val dir = usedPapersPath.split("/").apply(1)

  val newDir = "../"+dir.substring(0, dir.length-5)+"/"
  val baseDir = new File(newDir).mkdirs()

  lines.drop(1).foreach(line => {
    val pdfFile = new File(line.head)
    val dest = newDir+pdfFile.getName
    try {
      FileUtils.copyFile(pdfFile, new File(dest))
    } catch {
      case e: Exception => {
        logger.error(s"Cannot copy file ${line.head} to $dest directory!", e)
        None
      }
    }
  })

}
