package patmat

import scala.annotation.tailrec

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = {
    tree match {
      case Leaf(_, w) => w
      case Fork(l, r, _, _) => weight(l) + weight(r)
    }
  }

  def chars(tree: CodeTree): List[Char] = {
    tree match {
      case Leaf(c, _) => List(c)
      case Fork(l, r, _, _) => chars(l) ::: chars(r)
    }
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    @tailrec
    def loop(chars: List[Char], pairs: List[(Char, Int)]): List[(Char, Int)] = {
      chars match {
        case Nil => pairs
        case h :: t => loop(t, appendChar(h, pairs))
      }
    }

    def appendChar(c: Char, pairs: List[(Char, Int)]): List[(Char, Int)] = {
      pairs match {
        case Nil => List.empty
        case h :: t =>
          if (h._1 == c) (c, h._2 + 1) :: t
          else h :: appendChar(c, t)
      }
    }

    loop(chars, List.empty)
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.sortBy(_._2).map(p => Leaf(p._1, p._2))
//    def mergeSort(list: List[(Char, Int)]): List[(Char, Int)] = {
//      def merge(l1: List[(Char, Int)], l2: List[(Char, Int)]): List[(Char, Int)] = {
//        @tailrec
//        def loop(l1: List[(Char, Int)], acc: List[(Char, Int)]): List[(Char, Int)] = {
//          l1 match {
//            case Nil => acc
//            case h :: t => loop(t, appendOrdered(h, List.empty, acc))
//          }
//        }
//
//        @tailrec
//        def appendOrdered(p: (Char, Int), cur: List[(Char, Int)], acc: List[(Char, Int)]): List[(Char, Int)] = {
//          acc match {
//            case Nil => List(p)
//            case h :: _ if h._2 > p._2 => cur ::: p :: acc
//            case h :: t => appendOrdered(p, h :: cur, t)
//          }
//        }
//
//        loop(l1, l2)
//      }
//
//      if (list.length == 1) list
//      else {
//        val middle = list.length / 2
//        val nList = list.splitAt(middle)
//        merge(mergeSort(nList._1), mergeSort(nList._2))
//      }
//    }
//
//    mergeSort(freqs).map(p => Leaf(p._1, p._2))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def combineOrdered(node: CodeTree, trees: List[CodeTree]): List[CodeTree] = {
      trees match {
        case Nil => List(node)
        case h :: _ if weight(h) > weight(node) => node :: trees
        case h :: t => h :: combineOrdered(node, t)
      }
    }

    if (trees.length <= 2) trees
    else {
      trees match {
        case h1 :: h2 :: t => combineOrdered(makeCodeTree(h1, h2), t)
      }
    }
  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    @tailrec
    def loop(acc: List[CodeTree]): List[CodeTree] = {
      if (done(acc)) acc
      else loop(merge(acc))
    }

    loop(trees)
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))) match {
      case h :: _ => h
    }
  }


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    @tailrec
    def loop(bits: List[Bit], acc: List[Char], node: CodeTree): List[Char] = {
      bits match {
        case Nil => acc
        case h :: t =>
          findNode(h, node) match {
            case n: Leaf => loop(t, acc ::: List(n.char), tree)
            case n: Fork => loop(t, acc, n)
          }
      }
    }

    def findNode(bit: Bit, node: CodeTree): CodeTree = {
      node match {
        case n: Leaf => n
        case n: Fork =>
          bit match {
            case 0 => n.left
            case 1 => n.right
          }
      }
    }

    loop(bits, List.empty, tree)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def loop(text: List[Char], acc: List[Bit]): List[Bit] = {
      text match {
        case Nil => acc
        case h :: t => loop(t, acc ::: findChar(tree, h, List.empty))
      }
    }

    @tailrec
    def findChar(node: CodeTree, target: Char, acc: List[Bit]): List[Bit] = {
      def checkNode(node: CodeTree, target: Char) = {
        node match {
          case n: Leaf => n.char == target
          case n: Fork => n.chars.contains(target)
        }
      }

      node match {
        case _: Leaf => acc
        case n: Fork =>
          if (checkNode(n.left, target)) findChar(n.left, target, acc ::: List(0))
          else findChar(n.right, target, acc ::: List(1))
      }
    }

  loop(text, List.empty)
}

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table match {
      case h :: t =>
        if (h._1 == char) h._2
        else codeBits(t)(char)
    }
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def loop(node: CodeTree, cur: List[Bit]): CodeTable = {
      node match {
        case n: Fork => loop(n.right, cur ::: List(1)) ::: loop(n.left, cur ::: List(0))
        case n: Leaf => List((n.char, cur))
      }
    }

    loop(tree, List.empty)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    @tailrec
    def loop(table: CodeTable, acc: CodeTable): CodeTable = {
      table match {
        case Nil => acc
        case h :: t => loop(t, h :: acc)
      }
    }

    loop(a, b)
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def loop(cur: List[Char], table: CodeTable, acc: List[Bit]): List[Bit] = {
        cur match {
          case Nil => acc
          case h :: t => loop(t, table, acc ::: codeBits(table)(h))
        }
    }

    val table = convert(tree)
    loop(text, table, List.empty)
  }
}

object Huffman extends Huffman
