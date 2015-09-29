package utils

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.codehaus.plexus.util.FileUtils
import pdf.Permutation

/**
 * Created by mattia on 02.09.15.
 */
object Utils extends LazyLogging{

  val conf = ConfigFactory.load()
  val PERMUTATIONS_CSV_FILENAME = conf.getString("highlighter.permutationFilename")
  val ALLOWED_SINGLE_WORD_CHARS = conf.getInt("highlighter.allowedMaxLengthInWordMatch")

  def createCSV(permutations: List[Option[List[Permutation]]]) = {
    val writer = CSVWriter.open(new File(PERMUTATIONS_CSV_FILENAME))
    writer.writeRow(Seq("group_name", "method_index", "snippet_filename", "pdf_path", "method_on_top", "relative_height_top", "relative_height_bottom"))
    permutations.foreach(p => {
      if(p.isDefined) {
        p.get.foreach(pe => {
          val methodOnTop = if(pe.methodOnTop) 1 else 0
          writer.writeRow(Seq(pe.groupName,pe.methodIndex,pe.snippetPath,pe.pdfPath,methodOnTop,pe.relativeTop,pe.relativeBottom))
        })
      }
    })
    writer.close()
  }

  def escapeSearchString(searchString: String): List[String] = {
    /*val search = searchString.map(m => "\\Q" + m + "\\E" + "[\\-\\–\\—\\―\\n\\r]{0,5}\\s*").mkString("")
    if(searchString.length <= ALLOWED_SINGLE_WORD_CHARS || searchString.contains(" ")){
      "(?i)(\\b"+search+"\\b)"
    } else {
      "(?i)("+search+")"
    }*/
    if(searchString.length < 7) {
      permuteMethod(searchString).map(meth => {
        "(?i)(\\b"+meth.map(m => "\\Q" + m + "\\E[\\-\\–\\—\\―\\n\\r]{0,5}\\s*").mkString+"\\b)"
      })
    }
    else {
      List("(?i)("+searchString.replace(" ", "").map(m => "\\Q" + m + "\\E[\\-\\–\\—\\―\\n\\r]{0,5}\\s*").mkString+")")
    }
  }

  private def permuteMethod(t: String): List[String] = {
    val thingsToAddForSmallWords = " -.;,!(){}[]:".map(_.toString).toList
    thingsToAddForSmallWords.flatMap(before => {
      thingsToAddForSmallWords.map(after => {
        before + t + after
      })
    })
  }

  def copyAndMoveFile(dest: String, f: File, e: Exception): Any = {
    logger.error(s"Error while highlighting permutations for file $f", e)
    new File(dest).mkdir()
    val pdf = new File(dest + f.getName)

    try {
      FileUtils.copyFile(f, pdf)
    } catch {
      case e: Exception => {
        logger.error(s"Cannot copy file $f to $dest directory!", e)
        None
      }
    }
  }

  def emptyDirRecursively(dir: File): Boolean = {
    dir.listFiles().par.foreach(file => {
      if (file.isDirectory) {
        emptyDirRecursively(file)
      }
      file.delete()
    })
    true
  }

}
