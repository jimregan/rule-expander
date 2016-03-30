import java.io.IOException

import scala.collection.immutable.TreeMap
import scala.util.matching.Regex

sealed abstract class Treeish
case class DummyNonTerminal(lemma: String, tags: String) extends Treeish
case class DummyTerminal(chunk: String) extends Treeish
case class TLNonTerminal(lemma: String, tags: Array[String], pos: Array[Int]) extends Treeish
case class TLTerminal(chunk: String, pos:Int) extends Treeish
case class NonTerminal(lemma: String, tags: Array[String], align: Array[TLNonTerminal]) extends Treeish
case class Terminal(chunk: String, align: TLTerminal) extends Treeish


class GraphExpander {
  val cache = new collection.mutable.HashMap[String, List[Treeish]]()

  def addToCache(s: String, last: String, lineNumber: Int) = {
    val first = s.split(" = ")
    val chunkLabel = if (first(0).matches("^\\s*$")) last else first(0).trim
    val second = first(1).split(" \\| ")
    if (first.size != 2 || second.size != 3) {
      throw new IOException("Incorrect number of fields at line " + lineNumber + ": " + s)
    }
    if (chunkLabel.eq("")) {
      throw new IOException("Chunk label missing at line " + lineNumber + ": " + s)
    }
  }
}

object GraphExpander {
  /**
    * Generates a map of alignments, source side
    *
    * @param al space separated list of hyphen separated alignments
    */
  def splitAlignmentsSL(al: String) = {
    def toTuple(i: Array[Int]): (Int, Int) = (i(0), i(1))
    val als = al.split(" ").map{_.split("-").map(_.toInt)}.map{toTuple}
    val almap = als.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    almap
  }

  /**
    * Generates a map of alignments, translation side
    *
    * @param al space separated list of hyphen separated alignments
    */
  def splitAlignmentsTL(al: String) = {
    def toTuple(i: Array[Int]): (Int, Int) = (i(1), i(0))
    val als = al.split(" ").map{_.split("-").map(_.toInt)}.map{toTuple}
    val almap = als.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    almap
  }

  def makeToken(s: String): Treeish =  {
    val withLemma = new Regex("""\"([^"]*)\"<([^>]*)>""")
    val tagsOnly = new Regex("""([a-z][a-z0-9\.]*[a-z0-9])""")
    val nonTerminal = new Regex("""([A-Z][A-Z0-9]*[A-Z0-9])""")
    s match {
      case withLemma(lem, tag) => DummyNonTerminal(lem, tag)
      case tagsOnly(tag) => DummyNonTerminal("", tag)
      case nonTerminal(tag) => DummyTerminal(tag)
      case _ => DummyNonTerminal("", "")
    }
  }

  /**
    * @see dummyNTtoSL
    * @param a Array of TL tokens
    * @param m map of target-to-source alignment positions
    * @return map of target positions to source tokens tokens
    */
  def populateTLMap(a: Array[Treeish], m: Map[Int,Array[Int]]): Map[Int, Array[Treeish]] = {
    val msort = TreeMap(m.toSeq:_*)
    val mlast = msort.lastKey
    if (a.size != mlast) {
      System.err.println("Error: mismatching alignments")
    }
    def mkTLArray(ant: Array[Treeish], anum: Array[Int]): Array[Treeish] = {
      anum.map{i => ant(i - 1)}
    }
    a.zipWithIndex.map{x => (x._2, mkTLArray(a, m.get(x._2).get))}.toMap
  }

  /**
    * Make a TL side token
    * @param s string of the TL side token
    * @param pos position of token
    * @param m map of SL alignments
    * @return
    */
  def dummyNTtoTL(s: String, pos: Int, m: Map[Int, Array[Int]]): Treeish = {
    val dummy = makeToken(s)
    dummy match {
      case DummyNonTerminal(a, b) => TLNonTerminal(a, b.split("\\."), m.get(pos+1).get)
      case DummyTerminal(a) => {
        val psn = m.get(pos+1).get
        if(psn.size == 1) {
          TLTerminal(a, psn(0))
        } else {
          System.err.println("Unaligned terminal!!!")
          TLTerminal(a, 0)
        }
      }
    }
  }

  /**
    * Make an SL side token
    * @param s string of the SL side token
    * @param pos position of token
    * @param m map of TL tokens
    * @return
    */
  /*
  def dummyNTtoSL(s: String, pos: Int, m: Map[Int, Array[Treeish]]): Treeish = {
    val dummy = makeToken(s)
    NonTerminal(dummy.lemma, dummy.tags.split("\\."), m.getOrElse(pos+1, Array[TLNonTerminal]()))
    dummy match {
      case DummyNonTerminal(a, b) => TLNonTerminal(a, b.split("\\."), m.get(pos+1).get)
      case DummyTerminal(a) => {
        val psn = m.get(pos+1).get
        if(psn.size == 1) {
          TLTerminal(a, psn(0))
        } else {
          System.err.println("Unaligned terminal!!!")
          TLTerminal(a, 0)
        }
      }
    }
  }
  */
}
