/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 Trinity College, Dublin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package ie.tcd.sclcs.clcs

import scala.collection.immutable.TreeMap
import scala.util.matching.Regex

sealed abstract class Treeish
case class DummyNonTerminal(lemma: String, tags: String) extends Treeish
case class DummyTerminal(chunk: String) extends Treeish
case class TLNonTerminal(lemma: String, tags: Array[String], pos: Array[Int]) extends Treeish
case class TLTerminal(chunk: String, pos:Int) extends Treeish
case class SLNonTerminal(lemma: String, tags: Array[String], align: Array[TLNonTerminal]) extends Treeish
case class SLTerminal(chunk: String, align: TLTerminal) extends Treeish

class CastException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
class AlignmentException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
class ArgumentException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
class ParseException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

class RuleExpander {
  var cache = new collection.mutable.HashMap[String, collection.mutable.Set[Array[Treeish]]] with collection.mutable.MultiMap[String, Array[Treeish]]

  def addToCache(s: String, last: String, lineNumber: Int): String = {
    val first = s.split(" = ")
    val chunkLabel = if (first(0).matches("^\\s*$")) last else first(0).trim
    val second = first(1).split(" \\| ")
    if (first.size != 2 || second.size != 3) {
      throw new ParseException("Incorrect number of fields at line " + lineNumber + ": " + s)
    }
    if (chunkLabel.eq("")) {
      throw new ParseException("Chunk label missing at line " + lineNumber + ": " + s)
    }
    val SLDTokens = second(0).split(" ")
    val TLDTokens = second(1).split(" ")
    val SLAlign = RuleExpander.splitAlignmentsSL(second(2))
    val TLAlign = RuleExpander.splitAlignmentsTL(second(2))
    val TLTokens: Array[Treeish] = TLDTokens.zipWithIndex.map { x => RuleExpander.dummyNTtoTL(x._1, x._2 + 1, TLAlign) }
    val TLMap = RuleExpander.populateTLMap(TLTokens, TLAlign)
    val SLTokens: Array[Treeish] = SLDTokens.zipWithIndex.map { x => RuleExpander.dummyNTtoSL(x._1, x._2 + 1, TLMap) }
    cache.addBinding(chunkLabel, SLTokens)

    chunkLabel // to use in next call
  }

  def expandCache(i: Int) = {

  }
}

object RuleExpander {
  /**
    * Generates a map of alignments, source side
    *
    * @param al space separated list of hyphen separated alignments
    */
  def splitAlignmentsSL(al: String): Map[Int, Array[Int]] = {
    def toTuple(i: Array[Int]): (Int, Int) = (i(0), i(1))
    val als = al.split(" ").map {_.split("-").map(_.toInt)}.map{toTuple}
    val almap = als.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    almap
  }

  /**
    * Generates a map of alignments, translation side
    *
    * @param al space separated list of hyphen separated alignments
    */
  def splitAlignmentsTL(al: String): Map[Int, Array[Int]] = {
    def toTuple(i: Array[Int]): (Int, Int) = (i(1), i(0))
    val als = al.split(" ").map{_.split("-").map(_.toInt)}.map{toTuple}
    val almap = als.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
    almap
  }

  def makeToken(s: String): Treeish = {
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
  def populateTLMap(a: Array[Treeish], m: Map[Int, Array[Int]]): Map[Int, Array[Treeish]] = {
    def highest_val(i: Iterable[Array[Int]]): Int = {
      i.toSeq.map(_.toSeq).flatten.reduceLeft(_ max _)
    }
    val mlast = highest_val(m.values)
    if (a.size != mlast) {
      throw new AlignmentException("Alignment mismatch: " + a.size + " vs " + mlast)
    }
    def mkTLArray(ant: Array[Treeish], anum: Array[Int]): Array[Treeish] = {
      anum.map { i => ant(i - 1) }
    }
    m.map { x => (x._1, mkTLArray(a, x._2)) }
  }

  /**
    * Make a TL side token
    *
    * @param s   string of the TL side token
    * @param pos position of token
    * @param m   map of SL alignments
    * @return
    */
  def dummyNTtoTL(s: String, pos: Int, m: Map[Int, Array[Int]]): Treeish = {
    val dummy = makeToken(s)
    dummy match {
      case DummyNonTerminal(a, b) => TLNonTerminal(a, b.split("\\."), m.get(pos).get)
      case DummyTerminal(a) => {
        val psn = m.get(pos).get
        if (psn.size == 1) {
          TLTerminal(a, psn(0))
        } else {
          throw new AlignmentException("Unaligned Terminal")
        }
      }
      case _ => throw new ArgumentException("Invalid argument")
    }
  }

  def castTLTerminal(t: Treeish): TLTerminal = {
    t match {
      case tl: TLTerminal => tl
      case _ => throw new CastException("Expected TLTerminal")
    }
  }

  def castTLNonTerminal(t: Treeish): TLNonTerminal = {
    t match {
      case tl: TLNonTerminal => tl
      case _ => throw new CastException("Expected TLNonTerminal")
    }
  }

  def castSLTerminal(t: Treeish): SLTerminal = {
    t match {
      case tl: SLTerminal => tl
      case _ => throw new CastException("Expected SLTerminal")
    }
  }

  def castSLNonTerminal(t: Treeish): SLNonTerminal = {
    t match {
      case tl: SLNonTerminal => tl
      case _ => throw new CastException("Expected SLNonTerminal")
    }
  }

  /**
    * Make an SL side token
    *
    * @param s   string of the SL side token
    * @param pos position of token
    * @param m   map of TL tokens
    * @return
    */
  def dummyNTtoSL(s: String, pos: Int, m: Map[Int, Array[Treeish]]): Treeish = {
    val dummy = makeToken(s)
    def castTLNonTerminalArray(a: Array[Treeish]): Array[TLNonTerminal] = a.map {
      castTLNonTerminal
    }
    dummy match {
      case DummyNonTerminal(a, b) => SLNonTerminal(a, b.split("\\."), castTLNonTerminalArray(m.get(pos).get))
      case DummyTerminal(a) => {
        val psn = m.get(pos).get
        if (psn.size == 1) {
          SLTerminal(a, castTLTerminal(psn(0)))
        } else {
          throw new AlignmentException("Unaligned Terminal")
        }
      }
      case _ => throw new ArgumentException("Invalid argument")
    }
  }

  def TLNonTerminalToATTString(t: TLNonTerminal): String = {
    var out: String = ""
    if (t.tags.size > 0) {
      out = "^" + t.lemma + "<"
      out += t.tags.mkString("><")
      out += ">$"
    }
    out
  }
  def TLNonTerminalToRetratosString(t: TLNonTerminal): String = {
    var out = TLNonTerminalToATTString(t)
    if (out != "") {
      out += ":" + t.pos
    }
    out
  }
  def TLTokenToATTString(t: Treeish): String = {
    var out: String = ""
    t match {
      case SLTerminal(a, t) => {
        out = t.chunk + ":" + t.pos.toString
      }
      case SLNonTerminal(s, t, a) => {
        val str = a.map(TLNonTerminalToATTString)
      }
    }
    out
  }
  /**
   * Increment the positions of the TLTerminal contained
   * in the SLTerminal (taken from the position of an expanded
   * TLTerminal)
   */
  def incSLTerminal(s: SLTerminal, i: Int): SLTerminal = {
    SLTerminal(s.chunk, TLTerminal(s.align.chunk, s.align.pos + i))
  }
  /**
   * Increment the positions of the TLNonTerminal contained
   * in the SLNonTerminal (taken from the position of an expanded
   * TLTerminal)
   */
  def incSLNonTerminal(s: SLNonTerminal, i: Int): SLNonTerminal = {
    SLNonTerminal(s.lemma, s.tags, s.align.map{
      x => TLNonTerminal(x.lemma, x.tags, x.pos.map{a => a + i})
    })
  }
}
