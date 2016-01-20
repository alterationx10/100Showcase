package models

import java.text.SimpleDateFormat
import javax.xml.bind.DatatypeConverter

import akka.actor.ActorSystem
import com.google.inject.{AbstractModule, Inject, Singleton}
import modules.XboxAPI
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

//	"datePublished": "2016-01-18T01:52:56.6016817Z",

case class GameClipUri(uri: String, fileSize: Int, uriType: String, expiration: String) {
  def expirationEpoch = GameClipUri.sdf.parse(expiration).getTime
}
object GameClipUri {
  implicit val reads = Json.reads[GameClipUri]
  val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
}

case class GameClip(
                     gameClipId : String,
                     datePublished: Long,
                     thumbnail: String,
                     uri: String,
                     ownerXuid: String,
                     commentCount: Int,
                     likeCount: Int,
                     shareCount: Int,
                     expiration: Long
                   )  {
  val smallThumbnail = this.thumbnail.replace("Large","Small")

}

object GameClip {
  implicit val reads: Reads[GameClip] = (
    (JsPath \ "gameClipId").read[String] and
      (JsPath \ "datePublished").read[String].map(ts => DatatypeConverter.parseDateTime(ts).getTimeInMillis) and
      (JsPath \ "thumbnails").read[List[Thumbnail]].map(l => l.filter(_.thumbnailType.equalsIgnoreCase("Large")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "gameClipUris").read[List[GameClipUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "xuid").read[Long].map(_.toString) and
      (JsPath \ "commentCount").read[Int] and
      (JsPath \ "likeCount").read[Int] and
      (JsPath \ "shareCount").read[Int] and
      (JsPath \ "gameClipUris").read[List[GameClipUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.expirationEpoch).headOption.getOrElse(0.toLong))
    )(GameClip.apply _)
}

trait GameClipTable {
  protected val driver: JdbcProfile
  import driver.api._

  class GameClips(tag: Tag) extends Table[GameClip](tag, "clips") {

    def gameClipId: Rep[String] = column[String]("gameClipId", O.PrimaryKey)
    def datePublished: Rep[Long] = column[Long]("datePublished")
    def thumbnail: Rep[String] = column[String]("thumbnail")
    def uri: Rep[String] = column[String]("uri")
    def ownerXuid: Rep[String] = column[String]("ownerXuid")
    def commentCount: Rep[Int] = column[Int]("commentCount")
    def likeCount: Rep[Int] = column[Int]("likeCount")
    def shareCount: Rep[Int] = column[Int]("shareCount")
    def expiration: Rep[Long] = column[Long]("expiration")
    override def * : ProvenShape[GameClip] = (
      gameClipId, datePublished, thumbnail, uri, ownerXuid, commentCount, likeCount, shareCount, expiration
      ) <> ((GameClip.apply _).tupled, GameClip.unapply)
  }

  object GameClips {
    val query = TableQuery[GameClips]
    def createTable = query.schema.create
  }
}

@Singleton
class GameClipTableHelper @Inject()(dbConfigProvider: DatabaseConfigProvider, xboxAPI: XboxAPI, actorSystem: ActorSystem)
  extends GameClipTable with GamerTable {

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  override protected val driver: JdbcProfile = dbConfig.driver
  import driver.api._

  def onBoot = {
    dbConfig.db.run(MTable.getTables("clips")).map{ vector =>
      vector.toList.size match {
        case 0 => {
          Logger.info("Creating game clips table...")
          dbConfig.db.run(GameClips.createTable)
        }
        case _ => {
          Logger.info("Found existing game clips table...")
        }
      }
    }
  }

  def sync = {
    dbConfig.db.run(Gamers.query.result).map { gamers =>

      for {
        gamer <- gamers
      } yield {
        xboxAPI.gameClips(gamer).map {
          case Some(clips) => {
            val nonExpired = clips.filter(_.expiration > System.currentTimeMillis())
            Logger.info(s"Syncing ${nonExpired.size} clips for ${gamer.gt}")
            nonExpired.foreach { clip =>
              dbConfig.db.run(GameClips.query.insertOrUpdate(clip))
            }
          }
          case None => {
            Logger.info(s"Found 0 clips for ${gamer.gt} on xboxapi")
          }
        }
      }

    }
  }

  def prune = {
    val now = System.currentTimeMillis()
    val deleteQuery = GameClips.query.filter(rec => rec.expiration <= now)
    val deleteAction = deleteQuery.delete
    dbConfig.db.run(deleteAction).map {nDeleted =>
      Logger.info(s"Pruned $nDeleted expired clips.")
    }
  }

  onBoot

  actorSystem.scheduler.schedule(0 minutes, 60 minutes) {
    prune
    sync
  }

}

class GameClipTableModule extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[GameClipTableHelper]).asEagerSingleton()
  }

}