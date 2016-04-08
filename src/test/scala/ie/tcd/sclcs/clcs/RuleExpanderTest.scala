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

import org.scalatest._

class RuleExpanderTest extends FlatSpec with Matchers {
  "splitAlignmentsSL" should "generate a map of SL to TL alignments" in {
    val al1 = "1-1 1-2"
    val al1out = RuleExpander.splitAlignmentsSL(al1)
    assert (al1out.get(1).get === Array(1, 2))
    val al2 = "1-1 2-3 3-2"
    val al2out = RuleExpander.splitAlignmentsSL(al2)
    assert (al2out.get(1).get === Array(1))
    assert (al2out.get(2).get === Array(3))
    assert (al2out.get(3).get === Array(2))
  }

  "splitAlignmentsTL" should "generate a map of TL to SL alignments" in {
    val al1 = "1-1 1-2"
    val al1out = RuleExpander.splitAlignmentsTL(al1)
    assert (al1out.get(1).get === Array(1))
    assert (al1out.get(2).get === Array(1))
    val al2 = "1-1 2-2 3-2"
    val al2out = RuleExpander.splitAlignmentsTL(al2)
    assert (al2out.get(1).get === Array(1))
    assert (al2out.get(2).get === Array(2, 3))
  }

  "makeToken" should "generate a dummy token" in {
    assert(RuleExpander.makeToken("\"foo\"<bar>") === DummyNonTerminal("foo", "bar"))
    assert(RuleExpander.makeToken("bar") === DummyNonTerminal("", "bar"))
    assert(RuleExpander.makeToken("FOO") === DummyTerminal("FOO"))
  }

  "castTLNonTerminal" should "return a TLNonTerminal" in {
    val casted = RuleExpander.castTLNonTerminal(TLNonTerminal("", Array("foo"), Array(1)))
    casted shouldBe a [TLNonTerminal]
    a [CastException] should be thrownBy RuleExpander.castTLNonTerminal(TLTerminal("FOO", 1))
  }

  "castTLTerminal" should "return a TLTerminal" in {
    val casted = RuleExpander.castTLTerminal(TLTerminal("FOO", 1))
    casted shouldBe a [TLTerminal]
    a [CastException] should be thrownBy RuleExpander.castTLTerminal(TLNonTerminal("", Array("foo"), Array(1)))
  }

  "dummyNTtoTL" should "generate a TL token from a dummy token" in {
    val result = RuleExpander.dummyNTtoTL("foo.bar", 1, Map(1 -> Array(1, 2)))
    result shouldBe a [TLNonTerminal]
    val cast_result = RuleExpander.castTLNonTerminal(result)
    assert(cast_result.tags === Array("foo", "bar"))
    assert(cast_result.pos === Array(1, 2))
    assert(cast_result.lemma === "")
  }

  "dummyNTtoSL" should "generate an SL token from a dummy token" in {
    val result = RuleExpander.dummyNTtoSL("foo.bar", 1, Map(1 -> Array(TLNonTerminal("", Array("bat", "baz"), Array(1)))))
    result shouldBe a [SLNonTerminal]
    val cast_result = RuleExpander.castSLNonTerminal(result)
    assert(cast_result.tags === Array("foo", "bar"))
    assert(cast_result.lemma === "")
    val cast_res =  RuleExpander.castTLNonTerminal(cast_result.align(0))
    assert(cast_res.tags === Array("bat", "baz"))
    assert(cast_res.pos === Array(1))
  }

  "populateTLMap" should "fill an array of TL tokens based on alignments" in {
    def mycast(t: TLNonTerminal): Treeish = t
    val tlar = Array(mycast(TLNonTerminal("", Array("foo"), Array(1))), mycast(TLNonTerminal("", Array("bat", "baz"), Array(1))))
    val res = RuleExpander.populateTLMap(tlar, Map(1 -> Array(1, 2)))
    val first = res.get(1).get
    val firsta = RuleExpander.castTLNonTerminal(first(0))
    val firstb = RuleExpander.castTLNonTerminal(first(1))
    assert(firsta.tags === Array("foo"))
    assert(firstb.tags === Array("bat", "baz"))
  }

  "TLNonTerminalToATTString" should "create an apertium-transfer-tools string from a TLNonTerminal" in {
    var out = RuleExpander.TLNonTerminalToATTString(TLNonTerminal((""), Array("n", "sg"), Array(1)))
    assert(out == "^<n><sg>$")
    out = RuleExpander.TLNonTerminalToATTString(TLNonTerminal(("foo"), Array("n", "sg"), Array(1)))
    assert(out == "^foo<n><sg>$")
  }
}
