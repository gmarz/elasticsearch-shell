import org.jline.reader._
import org.jline.terminal._

object Shell extends App {

  println("\n"
    + """        __           __   _                                __  """ + "\n"
    + """  ___  / /___ _ ___ / /_ (_)____ ___ ___  ___ _ ____ ____ / /  """ + "\n"
    + """ / -_)/ // _ `/(_-</ __// // __/(_-</ -_)/ _ `// __// __// _ \ """ + "\n"
    + """ \__//_/ \_,_//___/\__//_/ \__//___/\__/ \_,_//_/   \__//_//_/ """ + "\n\n"
  )
  
  val settings = scala.collection.mutable.Map[String, String](
    "host" -> "http://localhost:9200"
  )

  val terminal = TerminalBuilder.terminal()
  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .completer(Completions.completer)
    .build()

  var read = true
  while(read) {
    try {
      eval(reader.readLine("elasticsearch> "))
    }
    catch {
      case e: UserInterruptException => read = false
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
    Parse.request(input) match { 
      case Left(request) => { 
        Client.response(request, settings.toMap) match {
          case Left(response) => {
            println(s"\n${request}")
            println(s"Status: ${response.code}\n")
            println(response.body) 
          }
          case Right(connectionError) => {
            println(s"\n${connectionError}\n") 
          }
        }
      }
      case Right(parseError) => {
        println(s"\n${parseError}\n") 
        usage()
      }
    }
  }

  def set(input: String) {
    Parse.setting(input) match {
      case Left(s) => settings(s._1) = s._2
      case Right(error) => {
        println(error + "\n")
        usage()
      }
    }
  }

  def usage() = println("TODO")

  def clear() {
    val ANSI_CLS = "\u001b[2J";
    val ANSI_HOME = "\u001b[H";
    System.out.print(ANSI_CLS + ANSI_HOME);
    System.out.flush();
  }

  def exit() = throw new UserInterruptException("")

}
