package utils

import java.io.{File, PrintWriter}

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

  def createCSV(permutations: List[Option[List[Permutation]]]) = {
    val writer = new PrintWriter(new File(PERMUTATIONS_CSV_FILENAME))
    writer.write("group_name, method_index, snippet_filename, pdf_path, method_on_top, relative_height_top, relative_height_bottom\n")
    permutations.foreach(p => {
      if(p.isDefined) {
        p.get.foreach(pe => {
          val methodOnTop = if(pe.methodOnTop) 1 else 0
          writer.append(pe.groupName + "," + pe.methodIndex+ "," + pe.snippetPath + "," + pe.pdfPath + "," + methodOnTop + "," +  pe.relativeTop +","+ pe.relativeBottom+"\n")
        })
      }
    })
    writer.close()
  }

  def escapeSearchString(allowedSingleWordCharacters: Int, searchString: String): String = {
    val search = searchString.replaceAll(" ", "").map(m => "\\Q" + m + "\\E" + "[\\-\\–\\—\\―\\n\\r]{0,5}\\s*").mkString("")
    if(searchString.length <= allowedSingleWordCharacters || searchString.contains(" ")){
      "(?i)(\\b"+search+"\\b)"
    } else {
      "(?i)("+search+")"
    }
  }

  def removePDFExtension(fileName: String): String = {
    fileName.substring(0, fileName.length - 4)
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
