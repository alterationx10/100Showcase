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

import scala.concurrent.ExecutionContext.Implicits.global

case class ScreenshotUri(uri: String, fileSize: Int, uriType: String, expiration: String) {
  def expirationEpoch = ScreenshotUri.sdf.parse(expiration).getTime
}
object ScreenshotUri {
  implicit val reads = Json.reads[ScreenshotUri]
  val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
}

case class Thumbnail(uri: String, fileSize: Int, thumbnailType: String)
object Thumbnail {
  implicit val reads = Json.reads[Thumbnail]
}


case class Screenshot(
                       screenshotId : String,
                       datePublished: Long,
                       thumbnail: String,
                       uri: String,
                       ownerXuid: String,
                       expiration: Long
                     ) {
  val smallThumbnail = this.thumbnail.replace("Large","Small")
}

object Screenshot {
  implicit val reads: Reads[Screenshot] = (
    (JsPath \ "screenshotId").read[String] and
      (JsPath \ "datePublished").read[String].map(ts => DatatypeConverter.parseDateTime(ts).getTimeInMillis) and
      (JsPath \ "thumbnails").read[List[Thumbnail]].map(l => l.filter(_.thumbnailType.equalsIgnoreCase("Large")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "screenshotUris").read[List[ScreenshotUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.uri.split("\\?")(0)).headOption.getOrElse("")) and
      (JsPath \ "xuid").read[Long].map(_.toString) and
      (JsPath \ "screenshotUris").read[List[ScreenshotUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.expirationEpoch).headOption.getOrElse(0.toLong))
    )(Screenshot.apply _)
}

trait ScreenShotTable {
  protected val driver: JdbcProfile
  import driver.api._

  class ScreenShots(tag: Tag) extends Table[Screenshot](tag, "screenshots") {

    def screenshotId: Rep[String] = column[String]("screenshotId", O.PrimaryKey)
    def datePublished: Rep[Long] = column[Long]("datePublished")
    def thumbnail: Rep[String] = column[String]("thumbnail")
    def uri: Rep[String] = column[String]("uri")
    def ownerXuid: Rep[String] = column[String]("ownerXuid")
    def expiration: Rep[Long] = column[Long]("expiration")

    override def * : ProvenShape[Screenshot] = (
      screenshotId, datePublished, thumbnail, uri, ownerXuid, expiration
      ) <> ((Screenshot.apply _).tupled, Screenshot.unapply)
  }

  object ScreenShots {
    val query = TableQuery[ScreenShots]
    def createTable = query.schema.create
  }
}

@Singleton
class ScreenShotTableHelper @Inject()(dbConfigProvider: DatabaseConfigProvider,  xboxAPI: XboxAPI, actorSystem: ActorSystem)
  extends ScreenShotTable with GamerTable {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  override protected val driver: JdbcProfile = dbConfig.driver
  import driver.api._

  def onBoot = {
    dbConfig.db.run(MTable.getTables("screenshots")).map{ vector =>
      vector.toList.size match {
        case 0 => {
          Logger.info("Creating screenshots table...")
          dbConfig.db.run(ScreenShots.createTable)
        }
        case _ => {
          Logger.info("Found existing screenshots table...")
        }
      }
    }
  }

  def sync = {
    dbConfig.db.run(Gamers.query.result).map { gamers =>

      for {
        gamer <- gamers
      } yield {
        xboxAPI.screenShots(gamer).map {
          case Some(sc) => {
            val nonExpired = sc.filter(_.expiration > System.currentTimeMillis())
            Logger.info(s"Syncing ${nonExpired.size} screenshots for ${gamer.gt}")
            nonExpired.foreach { s =>
              dbConfig.db.run(ScreenShots.query.insertOrUpdate(s))
            }
          }
          case None => {
            Logger.info(s"Found 0 clips for ${gamer.gt} on xboxapi")
          }
        }
      }

    }
  }

  def sync(gamer: Gamer) = {
    xboxAPI.screenShots(gamer).map {
      case Some(sc) => {
        val nonExpired = sc.filter(_.expiration > System.currentTimeMillis())
        Logger.info(s"Syncing ${nonExpired.size} screenshots for ${gamer.gt}")
        nonExpired.foreach { s =>
          dbConfig.db.run(ScreenShots.query.insertOrUpdate(s))
        }
      }
      case None => {
        Logger.info(s"Found 0 clips for ${gamer.gt} on xboxapi")
      }
    }
  }

  def prune = {
    val now = System.currentTimeMillis()
    val deleteQuery = ScreenShots.query.filter(rec => rec.expiration <= now)
    val deleteAction = deleteQuery.delete
    dbConfig.db.run(deleteAction).map {nDeleted =>
      Logger.info(s"Pruned $nDeleted expired screenshots.")
    }
  }

  onBoot

  actorSystem.scheduler.schedule(0 minutes, 60 minutes) {
    prune
    sync
  }

}

class ScreenShotTableModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[ScreenShotTableHelper]).asEagerSingleton()
  }
}