package modules

import java.io.{File, FileOutputStream}
import java.util.UUID

import akka.actor.{Actor, ActorSystem, Props}
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{CannedAccessControlList, PutObjectRequest}
import com.amazonaws.{AmazonClientException, AmazonServiceException}
import com.google.inject.{AbstractModule, Inject}
import models._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig}
import play.api.libs.iteratee.Iteratee
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AmazonS3 @Inject()(
                          configuration: Configuration,
                          wSClient: WSClient,
                          actorSystem: ActorSystem,
                          dbConfigProvider: DatabaseConfigProvider
                        ) {


  val accessKey: String = configuration.getString("showcase.aws.s3.access").getOrElse {
    throw new Exception("showcase.aws.s3.access not set in config")
  }

  val privateKey: String = configuration.getString("showcase.aws.s3.private").getOrElse {
    throw new Exception("showcase.aws.s3.private not set in config")
  }

  val bucketName: String = configuration.getString("showcase.aws.s3.bucket").getOrElse {
    throw new Exception("showcase.aws.s3.bucket not set in config")
  }

  val awsCredentials : BasicAWSCredentials = new BasicAWSCredentials(accessKey, privateKey)
  val awsClient = new AmazonS3Client(awsCredentials)

  if (!awsClient.doesBucketExist(bucketName)) {
    throw new Exception(s"S3 Bucket doesn't exist!")
  }

  val updateActor = actorSystem.actorOf(S3StoreActor.props(wSClient, dbConfigProvider, this))

  def saveScreenshot(screenshot: Screenshot): Unit = {

    updateActor ! ScreenshotUpdate(screenshot, "s")
    updateActor ! ScreenshotUpdate(screenshot, "l")
    updateActor ! ScreenshotUpdate(screenshot, "full")

  }

  def saveGameClip(gameClip: GameClip) = {
    updateActor ! GameClipUpdate(gameClip, "tn")
    updateActor ! GameClipUpdate(gameClip, "mp4")
  }

}

case class ScreenshotUpdate(screenshot: Screenshot, resource: String)
case class GameClipUpdate(gameClip: GameClip, resource: String)



class AmazonS3Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[AmazonS3])
  }
}

object S3StoreActor {
  def props(
             wSClient: WSClient,
             dbConfigProvider: DatabaseConfigProvider,
             amazonS3: AmazonS3
           ) = Props(new S3StoreActor(wSClient, dbConfigProvider, amazonS3))
}


class S3StoreActor(
                    wSClient: WSClient,
                    dbConfigProvider: DatabaseConfigProvider,
                    amazonS3: AmazonS3
                  )
  extends Actor with GameClipTable with ScreenShotTable with HasDatabaseConfig[JdbcProfile] {

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import driver.api._

  def downloadFile(url: String): Future[Option[File]] = {
    wSClient.url(url).getStream().map {
      case(response, body) => {
        response.status match {
          case 200 => {
            val tempFile = File.createTempFile(UUID.randomUUID().toString, UUID.randomUUID().toString)
            val outputStream = new FileOutputStream(tempFile)
            val iteratee = Iteratee.foreach[Array[Byte]] { bytes =>
              outputStream.write(bytes)
            }
            (body |>>> iteratee).andThen {
              case result =>
                // Close the output stream whether there was an error or not
                outputStream.close()
                // Get the result or rethrow the error
                result.get
            }.map(_ => tempFile).map(Some(_))
          }
          case _ => Future.successful(None)
        }
      }
      case _ => Future.successful(None)
    }.flatMap(identity)
  }

  override def receive: Receive = {



    case update: ScreenshotUpdate =>  update.resource match {

      case "s" => {
        downloadFile(update.screenshot.smallThumbnail).map {
          case Some(f) => {
            Logger.info(s"Saving file ${update.screenshot.smallThumbnail}")
            val newUrl = s"${update.screenshot.ownerXuid}/${update.screenshot.screenshotId}/${update.screenshot.screenshotId}_small.png"
            val por = new PutObjectRequest(amazonS3.bucketName,
              newUrl,
              f)
            por.setCannedAcl(CannedAccessControlList.PublicRead)
            try {
              amazonS3.awsClient.putObject(por)
              val q = for {
                s <- ScreenShots.query if s.screenshotId === update.screenshot.screenshotId
              } yield {
                s.smallThumbnail
              }
              dbConfig.db.run(q.update(s"https://s3.amazonaws.com/${amazonS3.bucketName}/$newUrl"))
            } catch {
              case ace: AmazonClientException => Logger.error(ace.getMessage)
              case ase: AmazonServiceException => Logger.error(ase.getMessage)
            }

          }
          case None => Logger.info(s"Unable to download file ${update.screenshot.smallThumbnail}")
        }
      }
      case "l" => {
        downloadFile(update.screenshot.largeThumbnail).map {
          case Some(f) => {
            Logger.info(s"Saving file ${update.screenshot.largeThumbnail}")
            val newUrl = s"${update.screenshot.ownerXuid}/${update.screenshot.screenshotId}/${update.screenshot.screenshotId}_large.png"
            val por = new PutObjectRequest(amazonS3.bucketName,
              newUrl,
              f)
            por.setCannedAcl(CannedAccessControlList.PublicRead)
            try {
              amazonS3.awsClient.putObject(por)
              val q = for {
                s <- ScreenShots.query if s.screenshotId === update.screenshot.screenshotId
              } yield {
                s.largeThumbnail
              }
              dbConfig.db.run(q.update(s"https://s3.amazonaws.com/${amazonS3.bucketName}/$newUrl"))
            } catch {
              case ace: AmazonClientException => Logger.error(ace.getMessage)
              case ase: AmazonServiceException => Logger.error(ase.getMessage)
            }

          }
          case None => Logger.info(s"Unable to download file ${update.screenshot.largeThumbnail}")
        }
      }
      case "full" => {
        downloadFile(update.screenshot.uri).map {
          case Some(f) => {
            Logger.info(s"Saving file ${update.screenshot.uri}")
            val newUrl = s"${update.screenshot.ownerXuid}/${update.screenshot.screenshotId}/${update.screenshot.screenshotId}.png"
            val por = new PutObjectRequest(amazonS3.bucketName,
              newUrl,
              f)
            por.setCannedAcl(CannedAccessControlList.PublicRead)
            try {
              amazonS3.awsClient.putObject(por)
              val q = for {
                s <- ScreenShots.query if s.screenshotId === update.screenshot.screenshotId
              } yield {
                s.smallThumbnail
              }
              dbConfig.db.run(q.update(s"https://s3.amazonaws.com/${amazonS3.bucketName}/$newUrl"))
            } catch {
              case ace: AmazonClientException => Logger.error(ace.getMessage)
              case ase: AmazonServiceException => Logger.error(ase.getMessage)
            }

          }
          case None => Logger.info(s"Unable to download file ${update.screenshot.uri}")
        }
      }
      case _ => {
        Logger.debug("Sent unknown message to self for Screenshot download")
      }

    } // Screenshots


    case update: GameClipUpdate => update.resource match {
      case "tn" => {
        downloadFile(update.gameClip.thumbnail).map {
          case Some(f) => {
            Logger.info(s"Saving file ${update.gameClip.thumbnail}")
            val newUrl = s"${update.gameClip.ownerXuid}/${update.gameClip.gameClipId}/${update.gameClip.gameClipId}.png"
            val por = new PutObjectRequest(amazonS3.bucketName,
              newUrl,
              f)
            por.setCannedAcl(CannedAccessControlList.PublicRead)
            try {
              amazonS3.awsClient.putObject(por)
              val q = for {
                c <- GameClips.query if c.gameClipId === update.gameClip.gameClipId
              } yield {
                c.thumbnail
              }
              dbConfig.db.run(q.update(s"https://s3.amazonaws.com/${amazonS3.bucketName}/$newUrl"))
            } catch {
              case ace: AmazonClientException => Logger.error(ace.getMessage)
              case ase: AmazonServiceException => Logger.error(ase.getMessage)
            }

          }
          case None => Logger.info(s"Unable to download file ${update.gameClip.thumbnail}")
        }
      }
      case "mp4" => {
        downloadFile(update.gameClip.uri).map {
          case Some(f) => {
            Logger.info(s"Saving file ${update.gameClip.uri}")
            val newUrl = s"${update.gameClip.ownerXuid}/${update.gameClip.gameClipId}/${update.gameClip.gameClipId}.mp4"
            val por = new PutObjectRequest(amazonS3.bucketName,
              newUrl,
              f)
            por.setCannedAcl(CannedAccessControlList.PublicRead)
            try {
              amazonS3.awsClient.putObject(por)
              val q = for {
                c <- GameClips.query if c.gameClipId === update.gameClip.gameClipId
              } yield {
                c.uri
              }
              dbConfig.db.run(q.update(s"https://s3.amazonaws.com/${amazonS3.bucketName}/$newUrl"))
            } catch {
              case ace: AmazonClientException => Logger.error(ace.getMessage)
              case ase: AmazonServiceException => Logger.error(ase.getMessage)
            }

          }
          case None => Logger.info(s"Unable to download file ${update.gameClip.uri}")
        }
      }
      case _ => {
        //
      }
    }


    case _ => {
      //
    }
  }
}