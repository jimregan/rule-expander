import java.io.IOException

sealed abstract class Treeish
case class TLNonTerminal(lemma: String, tags: String, pos: List[Int]) extends Treeish
case class TLTerminal(chunk: String, pos:Int) extends Treeish
case class NonTerminal(lemma: String, tags: String, align: List[TLNonTerminal]) extends Treeish
case class Terminal(chunk: String, align: List[TLTerminal]) extends Treeish


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
    * Generates a map of alignments, left to right
    *
    * @param String space separated list of hyphen separated alignments
    */
  def splitAlignmentsLR(al: String) = {
    def toTuple(i: Array[Int]): (Int, Int) = (i(0), i(1))
    val als = al.split(" ").map{_.split("_").map(_.toInt)}.map{toTuple}
    val almap = als.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    almap
  }

  /**
    * Generates a map of alignments, right to left
    *
    * @param String space separated list of hyphen separated alignments
    */
  def splitAlignmentsRL(al: String) = {
    def toTuple(i: Array[Int]): (Int, Int) = (i(1), i(0))
    val als = al.split(" ").map{_.split("_").map(_.toInt)}.map{toTuple}
    val almap = als.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    almap
  }
}
