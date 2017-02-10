
case class Error(message: String) {

  override def toString : String = {
    s"Error: ${message}"
  }

}

class ParseError extends Error("Failed to parse the provided input.")
