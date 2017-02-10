import scalaj.http._

object Client {

  def getResponse(r: Request) : HttpResponse[String] = {
    r.method.toUpperCase match {
      case "PUT" => put(r)
      case "POST" => post(r)
      case _ => request(r).asString
    }
  }

  def request(r: Request) : HttpRequest = {
    Http(s"http://localhost:9200/${r.path.stripPrefix("/")}")
      .method(r.method)
      .param("pretty", "true")
      .params(r.params)
      .header("content-type", "application/json")
  }

  def put(r: Request) : HttpResponse[String] = request(r).put(r.body).asString

  def post(r: Request) : HttpResponse[String] = request(r).postData(r.body).asString

}

