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
import play.api.libs.json._
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

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
                       largeThumbnail: String,
                       smallThumbnail: String,
                       url: String,
                       ownerXuid: String
                     )

object Screenshot {

  implicit val reads: Reads[Screenshot] = (
    (JsPath \ "screenshotId").read[String] and
      (JsPath \ "datePublished").read[String].map(ts => DatatypeConverter.parseDateTime(ts).getTimeInMillis) and
      (JsPath \ "thumbnails").read[List[Thumbnail]].map(l => l.filter(_.thumbnailType.equalsIgnoreCase("Large")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "thumbnails").read[List[Thumbnail]].map(l => l.filter(_.thumbnailType.equalsIgnoreCase("Small")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "screenshotUris").read[List[ScreenshotUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.uri.split("\\?")(0)).headOption.getOrElse("")) and
      (JsPath \ "xuid").read[Long].map(_.toString)
    )(Screenshot.apply _)
}

trait ScreenShotTable {
  protected val driver: JdbcProfile
  import driver.api._

  class ScreenShots(tag: Tag) extends Table[Screenshot](tag, "screenshots") {

    def screenshotId: Rep[String] = column[String]("screenshotId", O.PrimaryKey)
    def datePublished: Rep[Long] = column[Long]("datePublished")
    def largeThumbnail: Rep[String] = column[String]("largeThumbail")
    def smallThumbnail: Rep[String] = column[String]("smallThumbnail")
    def url: Rep[String] = column[String]("url")
    def ownerXuid: Rep[String] = column[String]("ownerXuid")

    override def * : ProvenShape[Screenshot] = (
      screenshotId, datePublished, largeThumbnail, smallThumbnail, url, ownerXuid
      ) <> ((Screenshot.apply _).tupled, Screenshot.unapply)
  }

  object ScreenShots {
    val query = TableQuery[ScreenShots]
    def createTable = query.schema.create

    def updateSmallThumbnail(screenshot: Screenshot, url: String) = {
      val q = for {
        s <- ScreenShots.query if s.screenshotId === screenshot.screenshotId
      } yield {
        s.smallThumbnail
      }
      q.update(url)
    }

    def updateLargeThumbnail(screenshot: Screenshot, url: String) = {
      val q =for {
        s <- ScreenShots.query if s.screenshotId === screenshot.screenshotId
      } yield {
        s.largeThumbnail
      }
      q.update(url)
    }

    def updateUrl(screenshot: Screenshot, url: String) = {
      val q = for {
        s <- ScreenShots.query if s.screenshotId === screenshot.screenshotId
      } yield {
        s.url
      }
      q.update(url)
    }
  }
}

@Singleton
class ScreenShotTableHelper @Inject()(
                                       dbConfigProvider: DatabaseConfigProvider,
                                       xboxAPI: XboxAPI,
                                       actorSystem: ActorSystem,
                                       @Named("s3upload") s3upload: ActorRef
                                     )
  extends ScreenShotTable with GamerTable {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  override protected val driver: JdbcProfile = dbConfig.driver
  import driver.api._

  def onBoot = {
    dbConfig.db.run(MTable.getTables("screenshots")).map { vector =>
      vector.toList.size match {
        case 0 => {
          Logger.info("Creating screenshots table...")
          dbConfig.db.run(ScreenShots.createTable)
        }
        case _ => {
          Logger.info("Found existing screenshots table...")
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
        xboxAPI.screenShots(gamer).map {
          case Some(ss) => {
            ss.foreach{ s =>
              dbConfig.db.run(ScreenShots.query.filter(_.screenshotId === s.screenshotId).result.headOption).map{
                case Some(exists) => {
                  Logger.info(s"Screenshot ${s.screenshotId} already exists for ${gamer.gt}")
                }
                case None => {
                  Logger.info(s"Inserting new Screenshot ${s.screenshotId} for ${gamer.gt}")
                  dbConfig.db.run(ScreenShots.query += s)
                  s3upload ! s
                }
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
    // rethink this
  }

  onBoot
}

class ScreenShotTableModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[ScreenShotTableHelper]).asEagerSingleton()
  }
}