package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import stats.ASEStatistics

class KConfigTest extends AssertionsForJUnit {

  @Test
  def defaultStrings() {
    val in =
    """
    config IFUPDOWN_UDHCPC_CMD_OPTIONS string {
     prompt "ifup udhcpc command line options" if [IFUPDOWN && UDHCPC]
     default ["-R -n"] if [IFUPDOWN && UDHCPC]
     depends on [IFUPDOWN && UDHCPC]
     inherited [IFUPDOWN && UDHCPC]
    }
    """

    val k = KConfigParser.parseKConfig(in)
    // Passed if it was able to parse
  }

  @Test
  def removeInherited1 {
    val in =
      """
      config IFUPDOWN_UDHCPC_CMD_OPTIONS string {
       prompt "ifup udhcpc command line options" if [IFUPDOWN && UDHCPC]
       default ["-R -n"] if [IFUPDOWN && UDHCPC]
       depends on [IFUPDOWN && UDHCPC]
       inherited [IFUPDOWN && UDHCPC]
      }
      """

    val ck = ASEStatistics.removeInherited(KConfigParser.parseKConfig(in))
    val ak = ck.toAbstractKConfig
    expect(Yes)(ak.findConfig("IFUPDOWN_UDHCPC_CMD_OPTIONS").get.pro)
    assert(ak.findConfig("IFUPDOWN_UDHCPC_CMD_OPTIONS").get.defs forall { _.currCondition == Yes})
  }

  @Test
  def removeInherited2 {
    val in =
      """
      config A tristate {
       prompt "..." if [B && C && (D || E)]
       inherited [B && C]

       config X tristate {
        prompt "..." if [A && B && C && D]
        inherited [A && B && C]
       }
      }
      """

    val ck = ASEStatistics.removeInherited(KConfigParser.parseKConfig(in))
    val ak = ck.toAbstractKConfig
    expect(Id("D") || Id("E"))(ak.findConfig("A").get.pro)
    expect(Id("D"))(ak.findConfig("X").get.pro)
  }

  @Test
  def removeDependsOn1 {
    val in =
      """
      config A tristate {
       prompt "..." if [B && C && (D || E)]
       depends on [B && C]

       config X tristate {
        prompt "..." if [A && B && C && D]
        depends on [A && B && C]
       }
      }
      """

    val ck = ASEStatistics.removeDependsOn(KConfigParser.parseKConfig(in))
    val ak = ck.toAbstractKConfig
    expect(Id("D") || Id("E"))(ak.findConfig("A").get.pro)
    expect(Id("D"))(ak.findConfig("X").get.pro)
  }

  // See resources/multipleDefinitions-1.Kconfig
  @Test
  def multipleDefinitions1() {
    val in = """
    config A boolean {
     prompt "Feature A" if []
    }
    config B boolean {
     prompt "Feature B" if []
    }
    config C boolean {
     prompt "Feature C" if []
    }
    config X tristate {
     default [y] if [A]
     default [n] if [B]
     default [y] if [C]
    }
    config X tristate {
     default [y] if [A]
     default [n] if [B]
     default [y] if [C]
    }
    config X tristate {
     default [y] if [A]
     default [n] if [B]
     default [y] if [C]
    }
    """
    val ak = KConfigParser.parseKConfig(in).toAbstractKConfig
    val defs = ak.findConfig("X").get.defs

    ak.configs foreach println

    // default y if A
    expect(Yes)(defs(0).iv)
    expect(Id("A"))(defs(0).currCondition)
    expect(List())(defs(0).prevConditions)

    // default n if B
    expect(No)(defs(1).iv)
    expect(Id("B"))(defs(1).currCondition)
    expect(List(Id("A")))(defs(1).prevConditions)

    // default y if C
    expect(Yes)(defs(2).iv)
    expect(Id("C"))(defs(2).currCondition)
    expect(List(Id("B"), Id("A")))(defs(2).prevConditions)
  }
}