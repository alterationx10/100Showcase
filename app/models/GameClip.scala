package models

import java.text.SimpleDateFormat
import javax.xml.bind.DatatypeConverter

import play.api.libs.functional.syntax._
import play.api.libs.json.{Json, JsPath, Reads}

//	"datePublished": "2016-01-18T01:52:56.6016817Z",

case class GameClipUri(uri: String, fileSize: Int, uriType: String, expiration: String) {
  def expirationEpoch = GameClipUri.sdf.parse(expiration).getTime
}
object GameClipUri {
  implicit val reads = Json.reads[GameClipUri]
  val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
}

case class GameClip(
                     gameClipId : String,
                     datePublished: Long,
                     thumbnail: String,
                     uri: String,
                     ownerXuid: Double,
                     commentCount: Int,
                     likeCount: Int,
                     shareCount: Int,
                     expiration: Long
                   )  {
  val smallThumbnail = this.thumbnail.replace("Large","Small")
}

object GameClip {

  implicit val reads: Reads[GameClip] = (
    (JsPath \ "gameClipId").read[String] and
      (JsPath \ "datePublished").read[String].map(ts => DatatypeConverter.parseDateTime(ts).getTimeInMillis) and
      (JsPath \ "thumbnails").read[List[Thumbnail]].map(l => l.filter(_.thumbnailType.equalsIgnoreCase("Large")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "gameClipUris").read[List[GameClipUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.uri).headOption.getOrElse("")) and
      (JsPath \ "xuid").read[Double] and
      (JsPath \ "commentCount").read[Int] and
      (JsPath \ "likeCount").read[Int] and
      (JsPath \ "shareCount").read[Int] and
      (JsPath \ "gameClipUris").read[List[GameClipUri]].map(l => l.filter(_.uriType.equalsIgnoreCase("Download")).map(_.expirationEpoch).headOption.getOrElse(0.toLong))
    )(GameClip.apply _)
}

