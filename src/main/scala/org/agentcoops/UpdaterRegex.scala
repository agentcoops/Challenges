import scalaz._
import Scalaz._
import scala.util.Random
import org.agentcoops.updaterRegex.trie._

/* Defines a state machine object model for a limited regular expression
 * grammar. State machine works with the trie data structure to find 
 * corrections for an input term.
 *
 * Library also offers rudimentary support capable of generating test cases
 * for application with CorrectionRegex.testCaseForWord(word: String) method.
 * 
 * Errors checked:
 * 1) capitalization errors: x -> (x|X)
 * 2) mistaken vowels: a -> (a|A|e|E|i|I|...)
 * 3) repeated characters: dd -> (d)+
 * 4) any combination: aA -> (a|A|e|E|i|I|...)+
 */
package org.agentcoops.updaterRegex {

// Defines a simple object model for Updater-regex state machine.
abstract trait RegexNode {
  val acceptableNextValues: Set[String]

  def nextStateForValue(currentTree: Tree[String]): RegexNode
  def validateNode(tree: Tree[String]) = 
    acceptableNextValues contains tree.rootLabel

  def potentialNextStates(currentTree: Tree[String]) = 
    currentTree.subForest filter validateNode

  def emitValue() = {
    val randomValue = Random.shuffle(acceptableNextValues.toList).head
    val newState = this.nextStateForValue(randomValue.node())
    (randomValue, newState)
  }
  
  // very simple, non-parameterized implementation---not tail recursive.
  def foldRight(initialValue: String)(f: (String, String) => String): String = 
    this match {
      case AcceptState() => initialValue
      case other => {
        val (value, newState) = this.emitValue()
        f(value, newState.foldRight(initialValue)(f))     
      }
    }
}

case class PlusStatePre(
  acceptableNextValues: Set[String], 
  next: RegexNode) 
extends RegexNode { 
  def nextStateForValue(currentTree: Tree[String]) =
    if (acceptableNextValues contains currentTree.rootLabel) 
      PlusStatePost(acceptableNextValues, next)
    else FailState()
}

case class PlusStatePost(
  acceptableNextValues: Set[String], 
  next: RegexNode) extends RegexNode {

  override def emitValue() = {
    val randomValue = 
      Random.shuffle(acceptableNextValues ++ next.acceptableNextValues toList).head
    val newState = nextStateForValue(randomValue.node())
    (randomValue, newState)
  }

  override def validateNode(tree: Tree[String]) = 
    super.validateNode(tree) || next.validateNode(tree)
  
  def nextStateForValue(currentTree: Tree[String]) = 
    if (acceptableNextValues contains currentTree.rootLabel)
      this
    else if (next.acceptableNextValues contains currentTree.rootLabel)
      next.nextStateForValue(currentTree)
    else FailState()
}

case class OrState(acceptableNextValues: Set[String], next: RegexNode) 
    extends RegexNode {
       
      def nextStateForValue(currentTree: Tree[String]) = 
         if (acceptableNextValues contains currentTree.rootLabel) next
         else FailState()
}

case class FailState() extends RegexNode {
  val acceptableNextValues = Set[String]()
  def nextStateForValue(currentTree: Tree[String]) = FailState()
}

case class AcceptState() extends RegexNode {
  val acceptableNextValues = Set[String]()
  override def validateNode(tree: Tree[String]) = true
  def nextStateForValue(currentTree: Tree[String]) = AcceptState()
}

object CorrectionRegex {
  // Core regex API.
  def apply(word: String) = new CorrectionRegex(word)

  def testCaseFromWord(word: String) = {
    // potentially add some duplicates to word.
    val newWord =   
      if (Random.nextBoolean)
        wordWithRandomDuplicates(word, Random.nextInt(word.length))
      else word
    val regex = compileRegex(newWord)
   
    // Sorry, terribly hacky implementation of extra credit. Basically, 
    // if the stack gets crazy, just return newWord. it's extra credit? 
    try {regex.foldRight("")((newValue, oldValue) => newValue + oldValue)}
    catch {case _: Throwable => newWord}
  }

  def testCasesFromWords(words: List[String]) = words map testCaseFromWord
  
  /* Very imparative implementation of the regex compiler. Goes backwards through
   * the input string, building up a regex for the cases indicated in problem:
   * 1) capitalization errors: x -> (x|X)
   * 2) mistaken vowels: a -> (a|A|e|E|i|I|...)
   * 3) repeated characters: dd -> (d)+
   * 4) any combination: aA -> (a|A|e|E|i|I|...)+
   *
   * The one case I was confused about in the grammar was repeated vowels, but from
   * the "FFoaaaoaoaoaoaoadd" test case I believe I am doing the correct thing 
   * represented in the regex for case four of the grammar.
   */
  def compileRegex(word: String): RegexNode = {
    val characters = word.split("") 
    var index = characters.length
    var value = ""
    var resultsSoFar: RegexNode = AcceptState()

    def peek() = characters(index-1)
    def prev() = { index = index - 1; value = characters(index) }
    def more() = if (index > 1) true else false

    while (more()) {
      prev()
      val values = generateCharacterOptions(value)

      if (peek() == value || (isVowel(value) && isVowel(peek()))) {
        resultsSoFar = PlusStatePre(values, resultsSoFar)
        while (peek() == value || (isVowel(value) && isVowel(peek()))) {
          prev()
        }
      } else {
        resultsSoFar = OrState(values, resultsSoFar)
      }
    }

    resultsSoFar
  }

  // Data and helper methods.
  val vowels = Set("a", "A", "e", "E", "i", "I", "o", "O", "u", "U")
  def isVowel(character: String): Boolean = 
    CorrectionRegex.vowels contains character 

  def generateCapitalOptions(character: String) = 
    Set(character.toUpperCase, character.toLowerCase)
    
  def generateCharacterOptions(character: String) = 
    if (isVowel(character)) CorrectionRegex.vowels 
    else generateCapitalOptions(character)  

  def wordWithRandomDuplicates(word: String, number: Int) = {
    var count = number
    val wordChars = word.split("").drop(1)
    val newWordChars = wordChars.foldRight(List[String]())(
      (newValue, oldValue) => {
        count = count-1
        newValue :: 
          (if (number > 0 && Random.nextBoolean)
            newValue :: oldValue
           else oldValue)
      }
    )

    val newWord = newWordChars.mkString("")
    newWord
  }
}

class CorrectionRegex(word: String) {
  import CorrectionRegex._

  val regex = compileRegex(word)
 
  /* Tail recursive continuation passing style implementation of a depth-first tree
   * traversal.  We're just interested in the first result we find, at which point we
   * exit the computation via the success function---otherwise, we call into the 
   * continuation which is either the next branch or the failure continuation.
   */
  def findCorrectionInDictionary(dictionary: Trie) = {
    // this is a bit sloppy---looking more into the scalaz treeloc zipper to clean.
    def success(collectedCharacters: List[String]) =  
      collectedCharacters.reverse.mkString("")
    
    def fail() = "NO SUGGESTION"

    def _findCorrectionInDictionary(
      collectString: List[String],
      dictionaryLocations: Stream[Tree[String]],
      regex: RegexNode,
      continuation: () => String 
    ): String = {      
      dictionaryLocations match {
        case Stream() => continuation()
        case currentLocation #:: futureBranches => // syntax for stream pattern match
          regex.nextStateForValue(currentLocation) match {
            case AcceptState() | PlusStatePost(_, AcceptState()) => 
              // leaf signifies that a valid word exists at this point in trie.
              if (Trie.hasLeafChild(currentLocation) 
                  || currentLocation.loc.isLeaf) 
                success(currentLocation.rootLabel :: collectString)
              else 
                // if not, look if any of the other branches are able to bring us success
                _findCorrectionInDictionary(
                  collectString,
                  futureBranches, 
                  regex,
                  continuation)

            // if this brought us to a fail state regex, pop up a level and try next branch
            case FailState() => continuation()

            case newState => 
              val possibleStates = newState.potentialNextStates(currentLocation) 
              // move down this branch for the new state. here we see the continuation
              // set to handle the other branches at the current level but with old
              // state, storing preceding levels and exit continuation in the ever
              // expanding continuation argument.
              _findCorrectionInDictionary(
                (currentLocation.rootLabel :: collectString),
                possibleStates,
                newState,
                {() => 
                  _findCorrectionInDictionary(
                    collectString,
                    futureBranches,
                    regex,
                    continuation)})
          }
      }
    }

    val root = dictionary.getRoot
    val states = regex.potentialNextStates(root)
        
    // we initialize with the fail continuation so that when all possible paths 
    // through trie for our input are exhausted we will return "NO SUGGESTION".
    _findCorrectionInDictionary(
      List(),
      states,
      regex,
      fail)
  } 
}

object SpellcheckTest {
  // test dictionary has all and only the correct words for test cases.
  val trie = Trie("sample.dict")

  def testWord(test: String, shouldBe: String) = {
    val correction = CorrectionRegex(test).findCorrectionInDictionary(trie)
    try {
      assert(correction == shouldBe)    
      println(f"$test succeeded!")
    } catch { 
      case e: Throwable => println(f"$test failed---$correction!") 
    }
  }

  // Very primitive "test suite."
  def runTests() = {
      println("Testing...")
      testWord("sandwich", "sandwich")
      testWord("jjoobbb", "job") 
      testWord("weke", "wake")
      testWord("CUNsperrICY", "conspiracy")
      testWord("ffoaoaoaoaoaoaaoaoaoaoaoadd", "food")
      testWord("sheeeeep", "sheep") 
      testWord("peepple", "people") 
      testWord("sheeple", "NO SUGGESTION")
      
      // make some more random tests for these words.
      List("sheep", "wake", "people", "job") foreach { word =>
        testWord(CorrectionRegex.testCaseFromWord(word), word)
      }

      println("Tests complete.")
  }
}

}
