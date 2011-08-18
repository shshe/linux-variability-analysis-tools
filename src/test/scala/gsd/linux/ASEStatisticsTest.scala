/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2011 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package gsd.linux

import org.junit.Test
import stats.ASEStatistics
import org.scalatest.junit.AssertionsForJUnit

class ASEStatisticsTest extends AssertionsForJUnit {

  implicit def toId(s: String) = Id(s)

  @Test def testRemoveInherited {
    val ck = ConcreteKConfig(CMenu(0, Prompt("Root"), List(
      CConfig(1, "A", defs = List(Default(Yes, "X")), inherited = "X"),
      CConfig(2, "B", defs = List(Default(Yes, And("X", "Y"))), inherited = "X", cs = List(
        CConfig(3, "C", sels = List(
          Select("S1", Or("X", "Y")),
          Select("S2", And(And("X", "Y"), "Z"))), inherited = "X") // condition shouldn't change
      )))))

    val ckPrime = ASEStatistics.removeInherited(ck)

    assert(ckPrime.configMap("A").defs.head.cond === Yes)
    assert(ckPrime.configMap("B").defs.head.cond === Id("Y"))
    assert(ckPrime.configMap("C").sels.head.cond === Or("X", "Y"))
    assert(ckPrime.configMap("C").sels(1).cond === And("Y", "Z"))
  }

  @Test def testRemoveDependsOn {
    val ck = ConcreteKConfig(CMenu(0, Prompt("Root"), List(
      CConfig(1, "A", defs = List(Default(Yes, "X")), depends = List(DependsOn("X"))),
      CConfig(2, "B", defs = List(Default(Yes, And("X", "Y"))), depends = List(DependsOn("X")), cs = List(
        CConfig(3, "C", sels = List(
          Select("S1", Or("X", "Y")),
          Select("S2", And(And("X", "Y"), "Z"))), depends = List(DependsOn("X"), DependsOn("Y")))
      )))))

    val ckPrime = ASEStatistics.removeDependsOn(ck)

    assert(ckPrime.configMap("A").defs.head.cond === Yes)
    assert(ckPrime.configMap("B").defs.head.cond === Id("Y"))
    assert(ckPrime.configMap("C").sels.head.cond === Or("X", "Y"))
    assert(ckPrime.configMap("C").sels(1).cond === Id("Z"))
  }

}