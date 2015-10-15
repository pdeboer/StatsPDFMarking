package input.bmc

import input.folder.FolderPDFSource
import pdf.PDFTextExtractor
import utils.Utils

import scala.ref.WeakReference

/**
 * Created by mattia on 28.09.15.
 */
object BMJLOCAL {

  lazy val texts: Map[Int, String] = {
    new FolderPDFSource("../bmc/").get().zipWithIndex.map(pdf => {
      pdf._2 -> PDFTextExtractor.extractTextAsString(pdf._1.getPath)
    }).toMap
  }

  def getPapersContainingTerm(term: String): List[BMJPaperBody] = {
    texts.map(txt => {
      if (Utils.buildRegexForString(term).exists(_.r.findFirstIn(txt._2).nonEmpty)) {
        Some(new BMJPaperBody(txt._1, new WeakReference(txt._2), "", -1))
      }else{
        None
      }
    }).filter(p => p.isDefined).map(_.get).toList
  }

  def getPaperBody(id: Int) = texts.getOrElse(id, null)
}

class BMJPaperBodyLocal(id: Int, var _body: WeakReference[String], url: String, year: Int) extends DBPaperBody(id, "", url, year) {
  override def body: String = _body.get.getOrElse({
    println("refetching " + id)
    val b: String = BMJLOCAL.getPaperBody(id)
    _body = WeakReference(b)
    b
  })

  override def equals(other: Any): Boolean = other match {
    case that: BMJPaperBody =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[BMJPaperBody]

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
