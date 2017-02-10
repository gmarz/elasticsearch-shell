object Shell extends App {
  var settings = scala.collection.mutable.Map[String, String]()

  override def main(args: Array[String]) {
    println("\n"
      + """        __           __   _                                __  """ + "\n"
      + """  ___  / /___ _ ___ / /_ (_)____ ___ ___  ___ _ ____ ____ / /  """ + "\n"
      + """ / -_)/ // _ `/(_-</ __// // __/(_-</ -_)/ _ `// __// __// _ \ """ + "\n"
      + """ \__//_/ \_,_//___/\__//_/ \__//___/\__/ \_,_//_/   \__//_//_/ """ + "\n\n"
    )

    //settings.put("host", "http://localhost:9200")

    var read = true
    while(read) {
      print("elasticsearch> ")
      val input = readLine()
      read = input != null
      if (read) eval(input)
    }
  }

  // _search
  // _search body
  // GET _search
  // GET _search body
  // set host http://localhost:9200
  // set user elastic
  // set password changeMe
  // help

  def eval(input: String) {
    var command = input.split(" ")(0)
    command.toLowerCase match {
      case "set" => set(input)
      case "help" => help()
      case "exit" => sys.exit(0)
      case _ => {
        Parser.request(input) match {
          case Left(request) => { 
            val response = Client.getResponse(request)
            println(s"\n${request}")
            println(s"Status: ${response.code}\n")
            println(response.body)
          }
          case Right(error) => {
            println(s"\n${error}\n")
            help()
          }
        }
      }
    }
  }

  def set(input: String) {
    println("TODO")
  }

  def help() {
    println("TODO")
  }
}
