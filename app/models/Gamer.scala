package models

import com.google.inject.{AbstractModule, Inject, Singleton}
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape

import scala.concurrent.ExecutionContext.Implicits.global

case class Gamer(gt: String, xuid: String, did: String)

trait GamerTable {
  protected val driver: JdbcProfile
  import driver.api._

  class Gamers(tag: Tag) extends Table[Gamer](tag, "gamers") {

    def gt: Rep[String] = column[String]("gt", O.PrimaryKey)
    def xuid: Rep[String] = column[String]("xuid")
    def did: Rep[String] = column[String]("did")

    override def * : ProvenShape[Gamer] = (gt, xuid, did) <> (Gamer.tupled, Gamer.unapply)

  }

  object Gamers {
    val query = TableQuery[Gamers]
    def createTable = query.schema.create
  }

}

@Singleton
class GamerTableHelper @Inject()(dbConfigProvider: DatabaseConfigProvider) extends GamerTable {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  override protected val driver: JdbcProfile = dbConfig.driver

  def onBoot = {
    dbConfig.db.run(MTable.getTables("gamers")).map{ vector =>
      vector.toList.size match {
        case 0 => {
          Logger.info("Creating gamers table...")
          dbConfig.db.run(Gamers.createTable)
        }
        case _ => {
          Logger.info("Found existing gamers table...")
        }
      }
    }
  }

  onBoot
}

class GamerTableModule extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[GamerTableHelper]).asEagerSingleton()
  }

}