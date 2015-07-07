package input

import java.io.File

/**
 * Created by pdeboer on 07/07/15.
 */
trait PDFSource {
	def get(): Iterable[File]
}
