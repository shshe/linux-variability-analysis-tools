package gsd.linux

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

class CombinationTest extends AssertionsForJUnit {
  @Test def simple {
    expect(Set(List(1), List(2)))(Combinations.choose(1, List(1,2)).toSet)
  }
}