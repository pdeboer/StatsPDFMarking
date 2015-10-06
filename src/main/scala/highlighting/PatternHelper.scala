package highlighting

import java.util.regex.Pattern

/**
 * Created by pdeboer on 06/10/15.
 */
object PatternHelper {
	def formatPatternWithWordBoundary(markingPattern: Pattern): Pattern = {
		Pattern.compile("(?i)(" + markingPattern.toString.substring(7, markingPattern.toString.length - 3) + ")")
	}
}
