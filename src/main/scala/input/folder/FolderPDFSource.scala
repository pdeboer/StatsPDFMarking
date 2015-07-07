package input.folder

import java.io.{File, FilenameFilter}

import input.PDFSource

/**
 * Created by pdeboer on 07/07/15.
 */
class FolderPDFSource(val path: String = ".") extends PDFSource {
	override def get(): Iterable[File] = new File(path).listFiles(new FilenameFilter {
		override def accept(dir: File, name: String): Boolean = name.endsWith(".pdf")
	})
}
