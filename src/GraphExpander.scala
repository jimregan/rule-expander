import java.io.IOException
import scala.util.matching.Regex

sealed abstract class Treeish
case class DummyNonTerminal(lemma: String, tags: String) extends Treeish
case class TLNonTerminal(lemma: String, tags: Array[String], pos: Array[Int]) extends Treeish
case class TLTerminal(chunk: String, pos:Int) extends Treeish
case class NonTerminal(lemma: String, tags: Array[String], align: List[TLNonTerminal]) extends Treeish
case class Terminal(chunk: String, align: TLTerminal) extends Treeish


class GraphExpander {
  //val cache = collection.mutable.Map[String, List[Treeish]]

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

  def makeNT(s: String): DummyNonTerminal =  {
    val withLemma = new Regex("""\"([^"]*)\"<([^>]*)>""")
    val tagsOnly = new Regex("""([a-z][a-z0-9\.]*[a-z0-9])""")
    s match {
      case withLemma(lem, tag) => DummyNonTerminal(lem, tag)
      case tagsOnly(tag) => DummyNonTerminal("", tag)
      case _ => DummyNonTerminal("", "")
    }
  }

  /**
    * Make a TL side token
    * @param s string of the TL side token
    * @param pos position of token
    * @param m map of SL alignments
    * @return
    */
  def dummyNTtoTL(s: String, pos: Int, m: Map[Int,Array[Int]]): TLNonTerminal = {
    val dummy = makeNT(s)
    TLNonTerminal(dummy.lemma, dummy.tags.split("\\."), m.getOrElse(pos+1, Array[Int]()))
  }
}
