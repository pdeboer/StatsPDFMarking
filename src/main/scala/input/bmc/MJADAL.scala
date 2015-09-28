package input.bmc

import scalikejdbc.{AutoSession, ConnectionPool, ConnectionPoolSettings, DB, _}

import scala.ref.WeakReference

/**
 * Created by pdeboer on 15/08/15.
 */
object MJADAL {
	Class.forName("com.mysql.jdbc.Driver")
	val settings = ConnectionPoolSettings(
		initialSize = 5,
		maxSize = 15,
		connectionTimeoutMillis = 3000L,
		validationQuery = "select 1 as one")
	ConnectionPool.singleton("jdbc:mysql://127.0.0.1/openreviewcrawl?autoReconnect=true", "root", "", settings)

	implicit val session = AutoSession

	def getPapersContainingTerm(term: String) = DB readOnly { implicit session =>
		val likeTerm = s"%${term.toLowerCase}%"

		sql"""SELECT DISTINCT p.id,  t.body, p.url, left(issue,4) AS y
		FROM mjapdftext t INNER JOIN bmjpaper p ON t.paperId = p.id
		WHERE LOWER(t.body) LIKE $likeTerm"""
			.map(r => new BMJPaperBody(r.int(1), WeakReference(r.string(2)), r.string(3), r.int(4))).list().apply()
	}

	def getPaperBody(id: Long) = DB readOnly { implicit session => sql"SELECT body FROM mjapdftext WHERE paperId = $id"
		.map(r => r.string(1)).single().apply().getOrElse(throw new IllegalArgumentException(s"couldnt find $id"))
	}

	def getPaperURL(id: Long) = DB readOnly { implicit session => sql"SELECT url, parentURL FROM mjapaper WHERE id = $id"
		.map(r => (r.string(1), r.string(2))).single().apply().getOrElse(throw new IllegalArgumentException(s"couldnt find $id"))
	}
}

class MJAPaperBody(id: Int, var _body: WeakReference[String], url: String, year: Int) extends DBPaperBody(id, "", url, year) {
	override def body: String = _body.get.getOrElse({
		println("refetching " + id)
		val b: String = MJADAL.getPaperBody(id)
		_body = WeakReference(b)
		b
	})

	override def equals(other: Any): Boolean = other match {
		case that: MJAPaperBody =>
			(that canEqual this) &&
				id == that.id
		case _ => false
	}

	override def canEqual(other: Any): Boolean = other.isInstanceOf[MJAPaperBody]

	override def hashCode(): Int = {
		val state = Seq(id)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}
}