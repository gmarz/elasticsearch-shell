import scalaj.http._

object Client {

  def response(r: Request, s: Map[String, String]) : HttpResponse[String] = {
    r.method.toUpperCase match {
      case "PUT" => put(r, s)
      case "POST" => post(r, s)
      case _ => request(r, s).asString
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

