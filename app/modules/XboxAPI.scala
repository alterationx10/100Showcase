package modules

import com.google.inject.{AbstractModule, Inject, Singleton}
import models.{Screenshot, GameClip, Gamer}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class XBoxAPIConfigException(msg: String) extends Exception(msg)

@Singleton
class XboxAPI @Inject() (configuration: Configuration, wSClient: WSClient) {

  val destinyTitleId = 247546985

  val apiKey : String = configuration.getString("showcase.xboxapi.key") match {
    case Some(api) => api
    case None => throw new XBoxAPIConfigException("value showcase.xboxapi.key not found in config file!")
  }

  val endpoint = "https://xboxapi.com"

  def gameClips(gamer: Gamer) : Future[Option[List[GameClip]]] = {

    val url = s"$endpoint/v2/${gamer.xuid}/game-clips/$destinyTitleId"

    wSClient.url(url).withHeaders(
      "X-AUTH" -> apiKey
    ).get.map {result =>

      val limitRemaining = result.header("X-RateLimit-Remaining").getOrElse("-1")
      Logger.debug(s"X-RateLimit-Remaining : $limitRemaining")
      val limitReset = result.header("X-RateLimit-Reset").getOrElse("-1")
      Logger.debug(s"X-RateLimit-Reset : $limitReset")

      result.status match {
        case 200 => {
          val array = (result.json).as[JsArray].as[List[JsObject]].flatMap { js =>
            js.validate[GameClip] match {
              case s: JsSuccess[GameClip] => Some(s.get)
              case _ => None
            }
          }
          Some(array)
        }
        case _ => {
          Logger.error(s"Error retrieving game clips from $url: STATUS ${result.status}")
          Logger.error(result.body)
          None
        }
      }
    }
  }

  def screenShots(gamer: Gamer) : Future[Option[List[Screenshot]]] = {

    val url = s"$endpoint/v2/${gamer.xuid}/screenshots/$destinyTitleId"

    wSClient.url(url).withHeaders(
      "X-AUTH" -> apiKey
    ).get.map {result =>

      val limitRemaining = result.header("X-RateLimit-Remaining").getOrElse("-1")
      Logger.debug(s"X-RateLimit-Remaining : $limitRemaining")
      val limitReset = result.header("X-RateLimit-Reset").getOrElse("-1")
      Logger.debug(s"X-RateLimit-Reset : $limitReset")

      result.status match {
        case 200 => {
          val array = (result.json).as[JsArray].as[List[JsObject]].flatMap { js =>
            js.validate[Screenshot] match {
              case s: JsSuccess[Screenshot] => Some(s.get)
              case _ => None
            }
          }
          Some(array)
        }
        case _ => {
          Logger.error(s"Error retrieving screenshots from $url: STATUS ${result.status}")
          Logger.error(result.body)
          None
        }
      }
    }
  }


}


class XboxAPIModule extends AbstractModule {
  override def configure(): Unit = bind(classOf[XboxAPIModule])
}

