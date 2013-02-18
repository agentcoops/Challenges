import scalaz._
import Scalaz._
import scala.io.Source

/* Defines a very rudimentary trie data structure using the scalaz tree 
 * library. Builds up the structure from a text file with one word per line.  
 */
package org.agentcoops.updaterRegex.trie {

  object Trie {
    def apply(loc: String) = {
      val trie = new Trie
      trie.loadFileLocation(loc)
      trie
    }
    
    def hasLeafChild(tree: Tree[String]) = 
      tree.loc.findChild(_.loc.isLeaf) match {
        case Some(_) => true
        case None => false
      }
  }
  
  class Trie {
    var tree = "".node()
    
    def listToTree(lst: List[String]) = lst match {
      case List() => "".leaf
      case hd::tl => 
        //descend from head of list, producing a single path tree of its values.
        tl.foldLeft(hd.node().loc)(
          (treeAccum, value) => { treeAccum.insertDownFirst(value.node()) } 
        ).insertDownFirst("".leaf) toTree 
    }
    
    def addWord(word: String) = {
      def _addWord(
        treeLoc: TreeLoc[String], 
        characters: List[String]): Tree[String] = characters match {
        
        case List() => treeLoc.insertDownFirst("".leaf).toTree
        case lst @ hd::tl => treeLoc.findChild(_.rootLabel == hd) match {
          case None => treeLoc.insertDownFirst(listToTree(lst)).toTree
          case Some(subTree) => _addWord(subTree, tl)
        }
      }
      
      tree = _addWord(tree.loc.root, word.split("").toList)
    }    
    
    def loadFileLocation(loc: String) =
      for (line <- Source.fromFile(loc).getLines()) {
        addWord(line)
      }
    
    def drawTree = tree.drawTree
    
    def getRoot = tree.loc.firstChild.get.tree
  }
}
