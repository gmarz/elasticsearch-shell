
case class Error(message: String) {

  override def toString : String = {
    s"Error: ${message}"
  }

}

class ParseError(message: String) extends Error(s"Failed to parse command - ${message}")

class UnknownSettingError(name: String) extends Error(s"Unknown setting: ${name}")

class ConnectionError(host: String, message: String) extends Error(s"Could not connect to host ${host} - ${message}")
