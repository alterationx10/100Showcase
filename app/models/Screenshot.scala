package models

import java.text.SimpleDateFormat
import javax.xml.bind.DatatypeConverter

import play.api.libs.json.{JsPath, Reads, Json}
import play.api.libs.functional.syntax._

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
                       thumbnail: String,
                       uri: String,
                       ownerXuid: Double,
                       expiration: Long
                     ) {
  val smallThumbnail = this.thumbnail.replace("Large","Small")
}

object Screenshot {

  implicit val reads: Reads[Screenshot] = (
    (JsPath \ "screenshotId").read[String] and
      (JsPath \ "datePublished").read[String].map(ts => DatatypeConverter.parseDateTime(ts).getTimeInMillis) and
      (JsPath \ "thumbnails").read[List[Thumbnail]].map(l => l.filter(_.thumbnailType.equalsIgnoreCase("Large")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "screenshotUris").read[List[ScreenshotUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "xuid").read[Double] and
      (JsPath \ "screenshotUris").read[List[ScreenshotUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.expirationEpoch).headOption.getOrElse(0.toLong))
    )(Screenshot.apply _)

}
