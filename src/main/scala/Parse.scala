
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
      case _ => Right(new ParseError("setting", value))
    }
  }

  private def host(value: String) : Either[(String, String), ParseError] = {
    if (value.startsWith("http://") || value.startsWith ("https://")) {
      Left(("host", value))
    } else {
      Right(new ParseError("a valid host", value))
    }
  }

  def request(value: String) : Either[Request, ParseError] = {
    val m = method(value)
    m match {
      case Left(method) => {
        val p = path(value)
        p match {
          case Left(path) => {
            val ps = params(path)
            ps match {
              case Left(params) => {
                val b = body(value)
                b match {
                  case Left(body) => Left(Request(method, path, params, body))
                  case Right(error) => Right(error)
                }
              }
              case Right(error) => Right(error)
            }
          }
          case Right(error) => Right(error)
        }
      }
      case Right(error) => Right(error)
    }
  }
  
  private def method(value: String) : Either[String, ParseError] = {
    val position = value.split(" ")
    val error = Right(new ParseError("HTTP method", value))
    val default = Left("GET")
    position.size match {
      case 1 => default
      case 2 => if (isMethod(position(0))) Left(position(0)) else default
      case 3 => if (isMethod(position(0))) Left(position(0)) else error
      case _ => error
    }
  }
 
  private def path(value: String) : Either[String, ParseError] = {
    val position = value.split(" ")
    var error = Right(new ParseError("path", value))
    position.size match {
      case 1 => if (isMethod(position(0))) error else Left(position(0))
      case 2 => if (isMethod(position(0))) Left(position(1)) else Left(position(0))
      case 3 => Left(position(1))
      case _ => error
    }
  }

  private def body(value: String) : Either[String, ParseError] = {
    val position = value.split(" ")
    val error = Right(new ParseError("request body", value))
    val default = Left("")
    position.size match {
      case 1 => default
      case 2 => if (isMethod(position(0))) default else Left(position(1))
      case 3 => Left(position(2))
      case _ => error
    }
  }

  private def params(value: String) : Either[Array[(String, String)], ParseError] = {
    var position = value.split("\\?")
    if (position.size == 1) {
      Left(Array[(String, String)]())
    } else {
      Left(position(1).split("&").map(p => p.split("=")).map(p => { 
        p.size match { 
          case 1 => (p(0), "true")
          case 2 => (p(0), p(1))
        }
      }))
    }
  }

  private def isMethod(value: String) : Boolean = {
    List("GET", "PUT", "POST", "DELETE", "HEAD").contains(value.toUpperCase)
  }

}
