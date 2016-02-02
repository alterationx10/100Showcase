package controllers

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

import com.google.inject.Inject
import models._
import modules.XboxAPI
import play.api.cache.CacheApi
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig}
import play.api.libs.json.{JsString, JsBoolean, JsObject}
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.{Configuration, Logger, Play}
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Application @Inject() (
                              wSClient: WSClient,
                              configuration: Configuration,
                              xboxAPI: XboxAPI,
                              gameClipTableHelper: GameClipTableHelper,
                              screenShotTableHelper: ScreenShotTableHelper,
                              cacheApi: CacheApi
                            ) extends Controller
  with GamerTable
  with GameClipTable
  with ScreenShotTable
  with HasDatabaseConfig[JdbcProfile]{

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)
  import driver.api._

  val bungieBaseUrl = "https://www.bungie.net/platform/destiny/"
  val destinyApiKey = configuration.getString("showcase.bungie.key").getOrElse("")

  def getPostParameter(postParam: String)(implicit request : Request[AnyContent]) : Option[String] = {
    request.body.asFormUrlEncoded.map(_.get(postParam).map(_.head toString)).flatten
  }

  def index = Action {
    Ok(views.html.index.render())
  }

  def gameClips = Action.async {
    dbConfig.db.run(Gamers.query.result).map { gamers =>
      val list = gamers.toList
      list.groupBy(_.xuid).map{case(k,v) => (k, v.head.gt)}
    }.map { gamerMap =>
      dbConfig.db.run(GameClips.query.result).map { result=>
        Ok(views.html.game_clips.render(result.toList.sortBy(- _.datePublished).take(99),gamerMap))
      }
    }.flatMap(identity)
  }

  def gamerGameClips(gt: String) = Action.async {
    dbConfig.db.run(Gamers.query.filter(_.gt === gt).result.headOption).map {
      case Some(g) => {
        dbConfig.db.run(GameClips.query.filter(_.ownerXuid === g.xuid).result).map { result =>
          Ok(views.html.game_clips.render(result.toList.sortBy(- _.datePublished), Map(g.xuid -> g.gt)))
        }
      }
      case None => {
        Future.successful{
          Ok(views.html.error.render("Gamertag not found!"))
        }
      }
    }.flatMap(identity)
  }

  def gamerGameClip(gt: String, id: String) = Action.async {
    dbConfig.db.run(Gamers.query.filter(_.gt === gt).result.headOption).map {
      case Some(g) => {
        dbConfig.db.run(GameClips.query.filter(_.gameClipId === id).result.headOption).map { result =>
          Ok(views.html.game_clip.render(result, g))
        }
      }
      case None => {
        Future.successful{
          Ok(views.html.error.render("Gamertag not found!"))
        }
      }
    }.flatMap(identity)
  }

  def screenShots = Action.async {
    dbConfig.db.run(Gamers.query.result).map { gamers =>
      val list = gamers.toList
      list.groupBy(_.xuid).map{case(k,v) => (k, v.head.gt)}
    }.map { gamerMap =>
      dbConfig.db.run(ScreenShots.query.result).map { result =>
        Ok(views.html.screenshots.render(result.toList.sortBy(- _.datePublished).take(99), gamerMap))
      }
    }.flatMap(identity)
  }

  def gamerScreenShots(gt: String) = Action.async {
    dbConfig.db.run(Gamers.query.filter(_.gt === gt).result.headOption).map {
      case Some(g) => {
        dbConfig.db.run(ScreenShots.query.filter(_.ownerXuid === g.xuid).result).map { result =>
          Ok(views.html.screenshots.render(result.toList.sortBy(- _.datePublished), Map(g.xuid -> g.gt)))
        }
      }
      case None => {
        Future.successful{
          Ok(views.html.error.render("Gamertag not found!"))
        }
      }
    }.flatMap(identity)
  }

  def gamerScreenShot(gt: String, scId: String) = Action.async {
    dbConfig.db.run(Gamers.query.filter(_.gt === gt).result.headOption).map {
      case Some(g) => {
        dbConfig.db.run(ScreenShots.query.filter(_.screenshotId === scId).result.headOption).map { result =>
          Ok(views.html.screenshot.render(result, g))
        }
      }
      case None => {
        Future.successful{
          Ok(views.html.error.render("Gamertag not found!"))
        }
      }
    }.flatMap(identity)
  }

  def gamerList = Action.async {
    dbConfig.db.run(Gamers.query.result).map { result=>
      Ok(views.html.gamers.render(result.toList.sortBy(_.gt)))
    }
  }

  def addGamer =  Action.async { implicit request =>

    val gamerTag = getPostParameter("gamertag").getOrElse("")

    destinyId(gamerTag).map {
      case Left(m) => Future.successful(Ok(views.html.error.render(m)))
      case Right(did) => {
        xboxAPI.xuid(gamerTag).map{
          case Some(xid) => {
            val gamer = Gamer(gamerTag, xid, did)
            dbConfig.db.run(Gamers.query.insertOrUpdate(gamer))
            gameClipTableHelper.sync(Some(gamer))
            screenShotTableHelper.sync(Some(gamer))
            Redirect(routes.Application.gamerList())
          }
          case None => Ok(views.html.error.render("Error looking up Gamertag on xboxapi"))
        }
      }
    }.flatMap(identity)
  }

  def about = Action {
    Ok(views.html.about.render())
  }

  def destinyId(gt: String): Future[Either[String, String]] = {
    val url = bungieBaseUrl + "TigerXBox/Stats/GetMembershipIdByDisplayName/" + gt.replaceAll(" ", "%20")
    wSClient.url(url)
      .withHeaders( "X-API-KEY" -> destinyApiKey ).get().map { response =>
      response.status match {
        case 200 => {
          val json = response.json
          (json \ "ErrorStatus").asOpt[String].getOrElse("") match {
            case "UserCannotResolveCentralAccount" => Left("No Destiny account for the user name entered.")
            case _ => {
              (json \ "Response").asOpt[String] match {
                case Some(id) => Right(id)
                case None => Left("BuffaloTauntaunApplePie: Error reading response from destiny servers.")
              }
            } //
          }
        }
        case _ => Left("AppleWombatFries: Error contacting destiny servers.")
      }
    }.map {
      case Right(did) => destinyClan(did).map {
        case Right(ct) => {
          ct match {
            case "E363" => Right(did)
            case _ => Left(s"$gt is not a member of the E363 clan ($ct)")
          }
        }
        case Left(e) => Left(e)
      }
      case Left(e) => Future.successful{
        Left(e)
      }
    }.flatMap(identity)
  }

  def destinyClan(did: String): Future[Either[String, String]] = {
    val url = bungieBaseUrl + "TigerXBox/Account/" + did
    wSClient.url(url)
      .withHeaders( "X-API-KEY" -> destinyApiKey ).get().map { response =>
      response.status match {
        case 200 => {
          (response.json \ "Response" \ "data" \ "clanTag").asOpt[String] match {
            case Some(ct) => Right(ct)
            case None => Left("TortiseManBearPig: Error contacting destiny servers.")
          }
        }
        case _ => Left("BumbleWaffleBee: Error contacting destiny servers.")
      }
    }
  }

  def postConfirm(implicit request: Request[AnyContent]): Boolean = {
    if (configuration.getString("showcase.refresh") == getPostParameter("sync")) true else false
  }

  // Set up some simple endpoints to trigger syncing
  def sync = Action { implicit request =>
    if (postConfirm) {
      Logger.info("Forcing sync...")
      gameClipTableHelper.sync()
      screenShotTableHelper.sync()
      Ok(JsObject(Seq("sync"->JsBoolean(true))))
    } else {
      Ok(JsObject(Seq("sync"->JsBoolean(false))))
    }
  }

  def syncClips = Action { implicit request =>
    if (postConfirm) {
      Logger.info("Forcing sync clips...")
      gameClipTableHelper.sync()
      Ok(JsObject(Seq("sync"->JsBoolean(true))))
    } else {
      Ok(JsObject(Seq("sync"->JsBoolean(false))))
    }
  }

  def syncScreenshots = Action { implicit request =>
    if (postConfirm) {
      Logger.info("Forcing sync screenshots...")
      screenShotTableHelper.sync()
      Ok(JsObject(Seq("sync"->JsBoolean(true))))
    } else {
      Ok(JsObject(Seq("sync"->JsBoolean(false))))
    }
  }

  // xbox live links expire.
  // sync won't update link if id is already in DB
  // Just delete, and let it re-sync later

  def migrateClips = Action.async {
    val q = GameClips.query.filter { clip =>
      List(
        clip.url like "%xboxlive%",
        clip.thumbnail like "%xboxlive%"
      ).reduceLeft(_ || _)
    }
    dbConfig.db.run(q.delete).map (result =>
      Ok(s"Pruned $result clips for re-syncing")
    )
  }

  def migrateScreenshots = Action.async {
    val q = ScreenShots.query.filter { screnshot =>
      List(
        screnshot.url like "%xboxlive%",
        screnshot.largeThumbnail like "%xboxlive%",
        screnshot.smallThumbnail like "%xboxlive%"
      ).reduceLeft(_ || _)
    }
    dbConfig.db.run(q.delete).map (result =>
      Ok(s"Pruned $result screenshots for re-syncing")
    )
  }

  val sdf = new SimpleDateFormat("dd-M-yyyy hh:mm:ss")
  sdf.setTimeZone(TimeZone.getTimeZone("America/New_York"))

  def lastSync = Action {
    val lastGc: Long = cacheApi.get[Long]("last-gc-sync").getOrElse(0)
    val lastSs: Long = cacheApi.get[Long]("last-ss-sync").getOrElse(0)

    val json = JsObject(Seq(
      "last-gc-sync" -> JsString(sdf.format(new Date(lastGc))),
      "last-ss-sync" -> JsString(sdf.format(new Date(lastSs)))
    ))

    Ok(json)
  }

}
