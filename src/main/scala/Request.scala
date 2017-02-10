
case class Request (method: String, path: String, params: Array[(String, String)], body: String) {

  override def toString : String = {
    s"Method: ${method.toUpperCase}\nPath: ${path}\nParams: ${params.mkString(" ")}\nBody: ${body}"
  }

}
