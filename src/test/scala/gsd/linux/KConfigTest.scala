package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import stats.ASEStatistics

class KConfigTest extends AssertionsForJUnit {

  @Test
  def defaultStrings {
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
    println(k)
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
}