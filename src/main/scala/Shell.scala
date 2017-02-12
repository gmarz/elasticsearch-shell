import org.jline.reader._
import org.jline.terminal._

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

    val terminal = TerminalBuilder.terminal()
    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .build()

    while(true) {
      try {
        eval(reader.readLine("elasticsearch> "))
      }
      catch {
        case e: UserInterruptException => sys.exit(0)
      }
    }

  }

  def eval(input: String) {
    var command = input.split(" ")(0)
    command.toLowerCase match {
      case "set" => set(input)
      case "help" => usage()
      case "clear" => clear()
      case "exit" => exit()
      case _ => request(input)
    }
  }

  def request(input: String) { 
    Parser.request(input) match { 
      case Left(request) => { 
        val response = Client.getResponse(request)
        println(s"\n${request}")
        println(s"Status: ${response.code}\n")
        println(response.body) 
      }
      case Right(error) => {
        println(s"\n${error}\n") 
        usage()
      }
    }
  }

  def clear() {
    val ANSI_CLS = "\u001b[2J";
    val ANSI_HOME = "\u001b[H";
    System.out.print(ANSI_CLS + ANSI_HOME);
    System.out.flush();
  }

  def set(input: String) = println("TODO")

  def usage() = println("TODO")

  def exit() = sys.exit(0)

}
