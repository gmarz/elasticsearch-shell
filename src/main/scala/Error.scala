
case class Error(message: String) {

  override def toString : String = {
    s"Error: ${message}"
  }

}

class ParseError(arg: String, from: String) extends Error(s"Failed while trying to parse ${arg} from '${from}'")
