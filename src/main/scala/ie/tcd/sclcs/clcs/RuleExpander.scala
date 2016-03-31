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
  val cache = new collection.mutable.HashMap[String, List[Treeish]]()

  def addToCache(s: String, last: String, lineNumber: Int) = {
    val first = s.split(" = ")
    val chunkLabel = if (first(0).matches("^\\s*$")) last else first(0).trim
    val second = first(1).split(" \\| ")
    if (first.size != 2 || second.size != 3) {
      throw new ParseException("Incorrect number of fields at line " + lineNumber + ": " + s)
    }
    if (chunkLabel.eq("")) {
      throw new ParseException("Chunk label missing at line " + lineNumber + ": " + s)
    }
  }
}

object RuleExpander {
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
      throw new AlignmentException("Alignment mismatch")
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
      case DummyNonTerminal(a, b) => TLNonTerminal(a, b.split("\\."), m.get(pos).get)
      case DummyTerminal(a) => {
        val psn = m.get(pos).get
        if(psn.size == 1) {
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
    * @param s string of the SL side token
    * @param pos position of token
    * @param m map of TL tokens
    * @return
    */
  def dummyNTtoSL(s: String, pos: Int, m: Map[Int, Array[Treeish]]): Treeish = {
    val dummy = makeToken(s)
    def castTLNonTerminalArray(a: Array[Treeish]): Array[TLNonTerminal] = a.map{castTLNonTerminal}
    dummy match {
      case DummyNonTerminal(a, b) => SLNonTerminal(a, b.split("\\."), castTLNonTerminalArray(m.get(pos).get))
      case DummyTerminal(a) => {
        val psn = m.get(pos).get
        if(psn.size == 1) {
          SLTerminal(a, castTLTerminal(psn(0)))
        } else {
          throw new AlignmentException("Unaligned Terminal")
        }
      }
      case _ => throw new ArgumentException("Invalid argument")
    }
  }
}
