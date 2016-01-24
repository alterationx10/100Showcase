package modules

import java.io.{FileOutputStream, File}
import java.util.UUID

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.google.inject.{AbstractModule, Inject}
import models.Screenshot
import play.api.{Logger, Configuration}
import play.api.libs.iteratee.Iteratee
import play.api.libs.ws.WSClient
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AmazonS3 @Inject()(configuration: Configuration, wSClient: WSClient) {

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

  def saveScreenshot(screenshot: Screenshot): Future[Boolean] = {
      // bucket/{id}/{id}_small.png
      // bucket/{id}/{id}_large.png
      // bucket/{id}/{id}.png
    downloadFile(screenshot.uri).map{
      case Some(file) => {
        awsClient.putObject(bucketName, s"${screenshot.screenshotId}/${screenshot.screenshotId}.png", file)
        file.delete()
        true
      }
      case None => {
        Logger.info(s"Couldn't create screenshot ${screenshot.screenshotId}")
        false
      }
    }

  }

}


class AmazonS3Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[AmazonS3])
  }
}
