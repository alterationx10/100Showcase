package models

import java.text.SimpleDateFormat
import javax.xml.bind.DatatypeConverter

import akka.actor.{ActorRef, ActorSystem}
import com.google.inject.name.Named
import com.google.inject.{AbstractModule, Inject, Singleton}
import modules.XboxAPI
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads}
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

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
                     url: String,
                     ownerXuid: String,
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
    def url: Rep[String] = column[String]("url")
    def ownerXuid: Rep[String] = column[String]("ownerXuid")
    def expiration: Rep[Long] = column[Long]("expiration")

    override def * : ProvenShape[GameClip] = (
      gameClipId, datePublished, thumbnail, url, ownerXuid, expiration
      ) <> ((GameClip.apply _).tupled, GameClip.unapply)
  }

  object GameClips {
    val query = TableQuery[GameClips]
    def createTable = query.schema.create

    def updateThumbnail(gameClip: GameClip, url: String) = {
      val q = for {
        c <- GameClips.query if c.gameClipId === gameClip.gameClipId
      } yield {
        c.thumbnail
      }
      q.update(url)
    }

    def updateUrl(gameClip: GameClip, url: String) = {
      val q = for {
        c <- GameClips.query if c.gameClipId === gameClip.gameClipId
      } yield {
        c.url
      }
      q.update(url)
    }
  }


}

@Singleton
class GameClipTableHelper @Inject()(
                                     dbConfigProvider: DatabaseConfigProvider,
                                     xboxAPI: XboxAPI,
                                     actorSystem: ActorSystem,
                                     @Named("s3upload") s3upload: ActorRef
                                   )
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
    }.map { _ =>
      actorSystem.scheduler.schedule(0 minutes, 60 minutes) {
        prune
        sync()
      }
    }
  }

  def sync(gamerOpt: Option[Gamer] = None) = {
    val gamersToSync = gamerOpt match {
      case Some(g) => Future.successful(Seq(g))
      case None => dbConfig.db.run(Gamers.query.result)
    }
    gamersToSync.map { gamers =>
      for {
        gamer <- gamers
      } yield {
        xboxAPI.gameClips(gamer).map {
          case Some(clips) => clips.foreach { c =>
            dbConfig.db.run(GameClips.query.filter(_.gameClipId === c.gameClipId).result.headOption).map{
              case Some(exists) => {
                Logger.info(s"Gameclip ${c.gameClipId} already exists for ${gamer.gt}")
              }
              case None => {
                Logger.info(s"Inserting new Gameclip ${c.gameClipId} for ${gamer.gt}")
                dbConfig.db.run(GameClips.query += c)
                s3upload ! c
              }
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
    // Rethink this
  }

  onBoot
}

class GameClipTableModule extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[GameClipTableHelper]).asEagerSingleton()
  }

}