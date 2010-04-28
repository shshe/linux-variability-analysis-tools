/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class HierarchyTest extends AssertionsForJUnit with KConfigBuilder with Hierarchy {
  @Test def testParentMapEasy {
    val k = mkBConfig("A", Nil, List(
      mkBConfig("B", Nil, List(
        mkBConfig("D", Nil, Nil))),
      mkBConfig("C", Nil, Nil)))

    val pMap = toIdMap(mkParentMap(mkRoot(List(k))), "_ROOT")
    assert(pMap("A") == "_ROOT")
    assert(pMap("B") == "A")
    assert(pMap("C") == "A")
    assert(pMap("D") == "B")
  }

  @Test def testParentMapWithMenus {
    val k = mkBConfig("A", Nil, List(
    mkMenu("Doesn't Matter", Yes, List(
      mkBConfig("B", Nil, List(
        mkBConfig("C", Nil, Nil)))))))

    val pMap = toIdMap(mkParentMap(mkRoot(List(k))), "_ROOT")
    assert(pMap("A") == "_ROOT")
    assert(pMap("B") == "A")
    assert(pMap("C") == "B")
  }

}
