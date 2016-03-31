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
}
