package input.bmc

import scalikejdbc.{AutoSession, ConnectionPool, DB, _}


/**
 * Created by pdeboer on 17/06/15.
 */
object BMCDAL {
	Class.forName("com.mysql.jdbc.Driver")
	ConnectionPool.singleton("jdbc:mysql://127.0.0.1/openreviewcrawl", "root", "")
	implicit val session = AutoSession

	def getPaperIDsWithTerms(term1:String, term2:String):List[DBPaper] = DB readOnly { implicit session =>

		val (likeTerm1, likeTerm2) = (s"%$term1%", s"%$term2%")

		sql"""select p.autoid, t.url as filename
		from papers p inner join prepub b on p.id1 = b.id1 and p.id2=b.id2 and p.id3=b.id3
		inner join pdftext t on b.imediaURL = t.url
		WHERE b.description = 'Original Submission - Version 1' and t.body like $likeTerm1 and t.body like $likeTerm2 """
			.map(r => DBPaper(r.long(1), r.string(2))).list().apply()
	}


	def getPapersContainingTerm(term: String) = DB readOnly { implicit session =>
		val likeTerm = s"%${term.toLowerCase}%"

		sql"""SELECT DISTINCT p.autoid, t.body, right(b.createdAt,4) as y
		FROM papers p INNER JOIN prepub b ON p.id1 = b.id1 AND p.id2=b.id2 AND p.id3=b.id3
		INNER JOIN pdftext t ON b.imediaURL = t.url
		WHERE b.description = 'Original Submission - Version 1' AND LOWER(t.body) LIKE $likeTerm"""
			.map(r => DBPaperBody(r.long(1))(r.string(2), r.int(3))).list().apply()
	}

	case class DBPaper(id: Long, filename: String)

	case class DBPaperBody(id: Long)(val body: String, val year: Int) extends Serializable

}
