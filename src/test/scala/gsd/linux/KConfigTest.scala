package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

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

}