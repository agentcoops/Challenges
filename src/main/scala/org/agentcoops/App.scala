import org.agentcoops.updaterRegex._
import org.agentcoops.updaterRegex.trie._

object CLISpellCheck extends App {

  override def main(args: Array[String]) = {
    val trie = Trie("sample.dict")
    
    while (true) {
      Console.print("> ")
      val word = Console.readLine.trim
      Console.println(CorrectionRegex(word).findCorrectionInDictionary(trie))
    }
  }
}
