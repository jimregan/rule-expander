import org.scalatest._

class RuleExpander extends FlatSpec with Matchers {
  "splitAlignmentsSL" should "generate a map of alignments" in {
    val al1 = "1-1 1-2"
    val al1out = RuleExpander.splitAlignmentsSL(al1)
  }
}
