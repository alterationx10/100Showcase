package actors

import java.io.{File, FileOutputStream}
import java.util.UUID
import java.util.concurrent.{TimeUnit, CountDownLatch}

import akka.actor.Actor
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{ObjectMetadata, CannedAccessControlList, PutObjectRequest}
import com.amazonaws.{AmazonClientException, AmazonServiceException}
import com.google.inject.{AbstractModule, Inject}
import models.{GameClip, GameClipTable, ScreenShotTable, Screenshot}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfig}
import play.api.libs.iteratee.Iteratee
import play.api.libs.ws.WSClient
import play.api.{Environment, Configuration, Logger}
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

  val CONTENT_TYPE_PNG = Some("image/png")
  val CONTENT_TYPE_MP4 = Some("video/mp4")

  @throws(classOf[AmazonClientException])
  @throws(classOf[AmazonServiceException])
  def uploadFile(fileOpt: Option[File], url: String, contentType: Option[String] = None) = {
    fileOpt match {
      case Some(f)  => {
        val por = new PutObjectRequest(bucketName, url, f)
        if (contentType.isDefined) {
          val md = new ObjectMetadata()
          md.setContentType(contentType.get)
          md.setCacheControl("max-age=31536000")
          por.withMetadata(md)
        }
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
          uploadFile(fileOpt, url, CONTENT_TYPE_PNG)
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
          uploadFile(fileOpt, tnLarge, CONTENT_TYPE_PNG)
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
          uploadFile(fileOpt, tnSmall, CONTENT_TYPE_PNG)
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
      latch.await(2, TimeUnit.MINUTES)
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
          uploadFile(fileOpt, tn, CONTENT_TYPE_PNG)
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
          uploadFile(fileOpt, mp4, CONTENT_TYPE_MP4)
          Logger.info(s"Updating database record: ${gc.gameClipId}")
          dbConfig.db.run(GameClips.updateUrl(gc,  s"https://s3.amazonaws.com/${bucketName}/$mp4"))
        } catch {
          case ace: AmazonClientException => println(ace.getMessage)
          case ase: AmazonClientException => println(ase.getMessage)
        } finally {
          if (fileOpt.isDefined) fileOpt.get.delete()
          latch.countDown()
        }
      }

      Logger.info(s"Pausing for Clip upload... : ${gc.gameClipId}")
      latch.await(5, TimeUnit.MINUTES)
      Logger.info(s"Clip upload complete... : ${gc.gameClipId}")

    }


    case _ => {
      Logger.debug("Unhandled S3UploadActor message")
    }
  }

}

class S3DummyActor extends Actor {
  override def receive: Actor.Receive = {
    case s : Any => Logger.debug(s"Message of type ${s.getClass.getName}")
    case _ => Logger.debug("Derp!")
  }
}

class S3UploadActorModule @Inject() (environment: Environment, configuration: Configuration) extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {

    val enabled = configuration.getBoolean("showcase.aws.s3.enable").getOrElse(false)

    if (enabled) {
      bindActor(classOf[S3UploadActor], "s3upload")
    } else {
      bindActor(classOf[S3DummyActor], "s3upload")
    }

  }
}
