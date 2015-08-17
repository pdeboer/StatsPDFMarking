package input.bmc

import scalikejdbc.{AutoSession, ConnectionPool, ConnectionPoolSettings, DB, _}

import scala.ref.WeakReference

/**
 * Created by pdeboer on 15/08/15.
 */
object BMJDAL {
	Class.forName("com.mysql.jdbc.Driver")
	val settings = ConnectionPoolSettings(
		initialSize = 5,
		maxSize = 15,
		connectionTimeoutMillis = 3000L,
		validationQuery = "select 1 as one")
	ConnectionPool.singleton("jdbc:mysql://127.0.0.1/openreviewcrawl?autoReconnect=true&failOverReadOnly=false&maxReconnects=10", "root", "", settings)

	implicit val session = AutoSession

	def getPapersContainingTerm(term: String) = DB readOnly { implicit session =>
		val likeTerm = s"%${term.toLowerCase}%"

		sql"""SELECT DISTINCT p.id, t.body, p.url, left(issue,4) AS y
		FROM bmjpdftext t INNER JOIN bmjpaper p
		WHERE LOWER(t.body) LIKE $likeTerm"""
			.map(r => BMJPaperBody(r.long(1))(WeakReference(r.string(2)), r.string(3), r.int(4))).list().apply()
	}

	def getPaperBody(id: Long) = DB readOnly { implicit session => sql"SELECT body FROM bmjpdftext WHERE paperId = $id"
		.map(r => r.string(1)).single().apply().get
	}
}

case class BMJPaperBody(override val id: Long)(var _body: WeakReference[String], url: String, year: Int) extends DBPaperBody(id)("", url, year) {
	override def body = _body.get.getOrElse({
		val b: String = BMJDAL.getPaperBody(id)
		_body = WeakReference(b)
		b
	})
}