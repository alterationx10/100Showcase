package controllers

import com.google.inject.Inject
import models.Gamer
import modules.XboxAPI
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
class Application @Inject() (xboxAPI: XboxAPI) extends Controller {


  def index = Action {

    Ok(views.html.index("Your new application is ready."))
  }

  def test = Action.async {
    val me = Gamer("Alteration x10", "2533274829464113", "")
    xboxAPI.gameClips(me).map {
      case Some(c) => {
        Ok(c.toString())
      }
      case None => Ok
    }
  }


  def gameClips = Action.async {
    val me = Gamer("Alteration x10", "2533274829464113", "")
    xboxAPI.gameClips(me).map{ gcList =>
      Ok(views.html.game_clips.render(gcList.getOrElse(List())))
    }
  }

  def screenShots = Action.async {
    val me = Gamer("Alteration x10", "2533274829464113", "")
    xboxAPI.screenShots(me).map{ scList =>
      Ok(views.html.screenshots.render(scList.getOrElse(List())))
    }
  }

  def gamerList = ???

  def addGamer = ???

  def about = ???

}
