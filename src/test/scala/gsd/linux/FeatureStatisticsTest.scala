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
import stats.FeatureStatistics
import org.scalatest.junit.AssertionsForJUnit

class FeatureStatisticsTest extends AssertionsForJUnit {

  def mkConcreteKConfig(children: CSymbol*) =
    ConcreteKConfig(CMenu(0, Prompt("Top Level Menu"), children.toList))

  def testStatistics(ck: ConcreteKConfig)(f: FeatureStatistics => Unit) =
    f(new FeatureStatistics(ck))


  @Test def testGroups {
    val stats1 = new FeatureStatistics(
      mkConcreteKConfig(CChoice(1, Prompt("Mutex"), true, false),
        CChoice(2, Prompt("Or"), false, true)))
    assert(stats1.mutexGroups.size === 1)
    assert(stats1.orGroups.size === 1)
    assert(stats1.xorGroups.size === 0)
    assert(stats1.optGroups.size === 0)

    val stats2 = new FeatureStatistics(
      mkConcreteKConfig(CChoice(1, Prompt("Xor"), true, true),
        CChoice(2, Prompt("Xor"), true, true)))
    assert(stats2.mutexGroups.size === 0)
    assert(stats2.orGroups.size === 0)
    assert(stats2.xorGroups.size === 2)
    assert(stats2.optGroups.size === 0)
  }

}