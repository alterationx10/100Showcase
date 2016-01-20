package controllers

import models.{GameClipTable, GamerTable, ScreenShotTable}
import play.api.Play
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig}
import play.api.mvc._
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global

class Application extends Controller
  with GamerTable
  with GameClipTable
  with ScreenShotTable
  with HasDatabaseConfig[JdbcProfile]{

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)
  import driver.api._

  def index = Action {
    Ok(views.html.index.render())
  }

  def gameClips = Action.async {
    dbConfig.db.run(Gamers.query.result).map { gamers =>
      val list = gamers.toList
      list.groupBy(_.xuid).map{case(k,v) => (k, v.head.gt)}
    }.map { gamerMap =>
      dbConfig.db.run(GameClips.query.result).map { result=>
        Ok(views.html.game_clips.render(result.toList.sortBy(- _.expiration),gamerMap))
      }
    }.flatMap(identity)
  }

  def screenShots = Action.async {
    dbConfig.db.run(Gamers.query.result).map { gamers =>
      val list = gamers.toList
      list.groupBy(_.xuid).map{case(k,v) => (k, v.head.gt)}
    }.map { gamerMap =>
      dbConfig.db.run(ScreenShots.query.result).map { result =>
        Ok(views.html.screenshots.render(result.toList.sortBy(- _.expiration), gamerMap))
      }
    }.flatMap(identity)
  }

  def gamerList = Action.async {
    dbConfig.db.run(Gamers.query.result).map { result=>
      Ok(views.html.gamers.render(result.toList.sortBy(_.gt)))
    }
  }

  def addGamer = ???

  def about = Action {
    Ok(views.html.about.render())
  }

}
