import scalaj.http._

object Client {

  def response(r: Request, settings: Map[String, String]) : Either[HttpResponse[String], Error] = {
    try {
      val responseAsString = r.method.toUpperCase match {
        case "PUT" => put(r, settings)
        case "POST" => post(r, settings)
        case _ => request(r, settings).asString
      }
      Left(responseAsString)
    } catch {
      case e: Exception => Right(new ConnectionError(settings("host"), s"${e.getMessage}"))
    }
  }

  private def request(r: Request, s: Map[String, String]) : HttpRequest = {
    Http(s"${s("host")}/${r.path.stripPrefix("/")}")
      .method(r.method)
      .param("pretty", "true")
      .params(r.params)
      .header("content-type", "application/json")
  }

  private def put(r: Request, s: Map[String, String]) : HttpResponse[String] = request(r, s).put(r.body).asString

  private def post(r: Request, s: Map[String, String]) : HttpResponse[String] = request(r, s).postData(r.body).asString

}

