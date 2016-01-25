package actors

import java.io.{File, FileOutputStream}
import java.util.UUID
import java.util.concurrent.CountDownLatch

import akka.actor.Actor
import com.amazonaws.{AmazonServiceException, AmazonClientException}
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{CannedAccessControlList, PutObjectRequest}
import com.google.inject.{AbstractModule, Inject}
import models.{GameClipTable, GameClip, ScreenShotTable, Screenshot}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig}
import play.api.libs.iteratee.Iteratee
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}
import play.libs.akka.AkkaGuiceSupport
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class S3UploadActor @Inject() (
                                configuration: Configuration,
                                wsClient: WSClient,
                                dbConfigProvider: DatabaseConfigProvider
                              )
  extends Actor
    with ScreenShotTable
    with GameClipTable
    with HasDatabaseConfig[JdbcProfile] {


  // Database
  override protected val dbConfig: DatabaseConfig[JdbcProfile] = dbConfigProvider.get[JdbcProfile]

  // Configure S3
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


  def downloadFile(url: String): Future[Option[File]] = {
    wsClient.url(url).getStream().map {
      case (response, body) => {
        response.status match {
          case 200 => {
            val tempFile = File.createTempFile(UUID.randomUUID().toString, null)
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

  @throws(classOf[AmazonClientException])
  @throws(classOf[AmazonServiceException])
  def uploadFile(fileOpt: Option[File], url: String) = {
    fileOpt match {
      case Some(f)  => {
        val por = new PutObjectRequest(bucketName, url, f)
        por.setCannedAcl(CannedAccessControlList.PublicRead)
        awsClient.putObject(por)
      }
      case None => //
    }
  }

  override def receive: Receive = {

    case ss: Screenshot => {

      // Try and not kill the memory on our free/cheap servers
      val latch = new CountDownLatch(3)

      // Main file
      val base = s"${ss.ownerXuid}/${ss.screenshotId}/${ss.screenshotId}"
      val url = s"${base}.png"
      val tnSmall = s"${base}_small.png"
      val tnLarge = s"${base}_large.png"

      Logger.info(s"Downloading file from ${ss.url}: ${ss.screenshotId}")
      downloadFile(ss.url).map{ fileOpt =>
        try {
          Logger.info(s"Uploading file to ${url}: ${ss.screenshotId}")
          uploadFile(fileOpt, url)
          Logger.info(s"Updating database record: ${ss.screenshotId}")
          dbConfig.db.run(ScreenShots.updateUrl(ss, s"https://s3.amazonaws.com/${bucketName}/$url"))
        } catch {
          case ace: AmazonClientException => println(ace.getMessage)
          case ase: AmazonClientException => println(ase.getMessage)
        } finally {
          if (fileOpt.isDefined) fileOpt.get.delete()
          latch.countDown()
        }
      }

      Logger.info(s"Downloading file from ${ss.largeThumbnail}: ${ss.screenshotId}")
      downloadFile(ss.largeThumbnail).map{ fileOpt =>
        try {
          Logger.info(s"Uploading file to ${tnLarge}: ${ss.screenshotId}")
          uploadFile(fileOpt, tnLarge)
          Logger.info(s"Updating database record: ${ss.screenshotId}")
          dbConfig.db.run(ScreenShots.updateLargeThumbnail(ss, s"https://s3.amazonaws.com/${bucketName}/$tnLarge"))
        } catch {
          case ace: AmazonClientException => println(ace.getMessage)
          case ase: AmazonClientException => println(ase.getMessage)
        } finally {
          if (fileOpt.isDefined) fileOpt.get.delete()
          latch.countDown()
        }
      }

      Logger.info(s"Downloading file from ${ss.smallThumbnail}: ${ss.screenshotId}")
      downloadFile(ss.smallThumbnail).map { fileOpt =>
        try {
          Logger.info(s"Uploading file to ${tnSmall}: ${ss.screenshotId}")
          uploadFile(fileOpt, tnSmall)
          Logger.info(s"Updating database record: ${ss.screenshotId}")
          dbConfig.db.run(ScreenShots.updateSmallThumbnail(ss, s"https://s3.amazonaws.com/${bucketName}/$tnSmall"))
        } catch {
          case ace: AmazonClientException => println(ace.getMessage)
          case ase: AmazonClientException => println(ase.getMessage)
        } finally {
          if (fileOpt.isDefined) fileOpt.get.delete()
          latch.countDown()
        }
      }
      Logger.info(s"Pausing for Screenshot upload: ${ss.screenshotId}")
      latch.await()
      Logger.info(s"Screenshot upload complete: ${ss.screenshotId}")

    }

    case gc: GameClip => {

      // Try and not kill the memory on our free/cheap servers
      val latch = new CountDownLatch(2)

      val base = s"${gc.ownerXuid}/${gc.gameClipId}/${gc.gameClipId}"
      val tn = s"$base.png"
      val mp4 = s"$base.mp4"

      Logger.info(s"Downloading file from ${gc.thumbnail}: ${gc.gameClipId}")
      downloadFile(gc.thumbnail).map{ fileOpt =>
        try {
          Logger.info(s"Uploading file to ${tn}: ${gc.gameClipId}")
          uploadFile(fileOpt, tn)
          Logger.info(s"Updating database record: ${gc.gameClipId}")
          dbConfig.db.run(GameClips.updateThumbnail(gc,  s"https://s3.amazonaws.com/${bucketName}/$tn"))
        } catch {
          case ace: AmazonClientException => println(ace.getMessage)
          case ase: AmazonClientException => println(ase.getMessage)
        } finally {
          if (fileOpt.isDefined) fileOpt.get.delete()
          latch.countDown()
        }
      }

      Logger.info(s"Downloading file from ${gc.url}: ${gc.gameClipId}")
      downloadFile(gc.url).map{ fileOpt =>
        try {
          Logger.info(s"Uploading file to ${mp4}: ${gc.gameClipId}")
          uploadFile(fileOpt, mp4)
          Logger.info(s"Updating database record: ${gc.gameClipId}")
          dbConfig.db.run(GameClips.updateThumbnail(gc,  s"https://s3.amazonaws.com/${bucketName}/$mp4"))
        } catch {
          case ace: AmazonClientException => println(ace.getMessage)
          case ase: AmazonClientException => println(ase.getMessage)
        } finally {
          if (fileOpt.isDefined) fileOpt.get.delete()
          latch.countDown()
        }
      }

      Logger.info(s"Pausing for Clip upload... : ${gc.gameClipId}")
      latch.await()
      Logger.info(s"Clip upload complete... : ${gc.gameClipId}")

    }


    case _ => {
      Logger.debug("Unhandled S3UploadActor message")
    }
  }

}

class S3UploadActorModule extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
    bindActor(classOf[S3UploadActor], "s3upload")
  }
}
