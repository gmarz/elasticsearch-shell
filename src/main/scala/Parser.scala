object Parser {
 
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
  
  def method(value: String) : Either[String, ParseError] = {
    var parts = value.split(" ")
    parts.size match {
      case 1 => Left("GET")
      case 2 => if (isMethod(parts(0))) Left(parts(0)) else Left("GET")
      case 3 => if (isMethod(parts(0))) Left(parts(0)) else Right(new ParseError)
      case _ => Right(new ParseError)
    }
  }
 
  def path(value: String) : Either[String, ParseError] = {
    var parts = value.split(" ")
    parts.size match {
      case 1 => if (isMethod(parts(0))) Right(new ParseError) else Left(parts(0))
      case 2 => if (isMethod(parts(0))) Left(parts(1)) else Left(parts(0))
      case 3 => Left(parts(1))
      case _ => Right(new ParseError)
    }
  }

  def body(value: String) : Either[String, ParseError] = {
    var parts = value.split(" ")
    parts.size match {
      case 1 => Left("")
      case 2 => if (isMethod(parts(0))) Left("") else Left(parts(1))
      case 3 => Left(parts(2))
      case _ => Right(new ParseError)
    }
  }

  def params(value: String) : Either[Array[(String, String)], ParseError] = {
    var parts = value.split("\\?")
    if (parts.size == 1) {
      Left(Array[(String, String)]())
    } else {
      Left(parts(1).split("&").map(p => p.split("=")).map(p => { 
        p.size match { 
          case 1 => (p(0), "true")
          case 2 => (p(0), p(1))
        }
      }))
    }
  }

  def isMethod(value: String) : Boolean = {
    List("get", "put", "post", "delete", "head").contains(value.toLowerCase)
  }

}
