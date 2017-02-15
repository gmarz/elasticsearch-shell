
object Parse {
 
  def setting(value: String) : Either[(String, String), Error] = {
    val parts = value.split(" ")
    parts.size match {
      case 3 => {
        val k = parts(1)
        val v = parts(2)
        k match {
          case "host" => host(v)
          case _ => Right(new UnknownSettingError(k))
        }
      }
      case _ => Right(new ParseError("Invalid setting"))
    }
  }
 
  def request(value: String) : Either[Request, ParseError] = {
    val parts = value.split(" ")
    if (parts.size >= 2) {
      val method = parts(0)
      if (validHttpVerb(method)) {
        val path = parts(1)
        if (validPath(path)) {
          val queryParams = params(path)
          val body = parts.drop(2).mkString("")
          Left(Request(method, path, queryParams, body))
        } else Right(new ParseError("Invalid path"))
      } else Right(new ParseError("Invalid HTTP method"))
    } else Right(new ParseError("Invalid length"))
  }
  
  private def host(value: String) : Either[(String, String), ParseError] = {
    if (value.startsWith("http://") || value.startsWith ("https://")) {
      Left(("host", value))
    } else {
      Right(new ParseError("Invalid host"))
    }
  }

  private def params(value: String) : Array[(String, String)] = {
    var position = value.split("\\?")
    if (position.size == 1) {
      Array[(String, String)]()
    } else {
      position(1).split("&").map(p => p.split("=")).map(p => { 
        p.size match { 
          case 1 => (p(0), "true")
          case 2 => (p(0), p(1))
        }
      })
    }
  }

  private def validHttpVerb(value: String) : Boolean = {
    List("GET", "PUT", "POST", "DELETE", "HEAD").contains(value.toUpperCase)
  }
 
  // TODO
  private def validPath(value: String) : Boolean =  true

}
