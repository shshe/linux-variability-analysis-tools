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

import Hierarchy._

class HierarchyTest extends AssertionsForJUnit {

  val ROOT_NAME = "_ROOT"

  @Test def testParentMapEasy {
    val k =
      ConcreteKConfig(
        CMenu(1, Prompt(ROOT_NAME), List(
          CConfig(2, "A", cs = List(
            CConfig(3, "B", cs = List(
              CConfig(4, "D"))),
            CConfig(5, "C"))))))

    val pMap = toStringMap(mkConfigMap(k), k.allConfigs, "_ROOT")
    assert(pMap("A") == "_ROOT")
    assert(pMap("B") == "A")
    assert(pMap("C") == "A")
    assert(pMap("D") == "B")
  }

  @Test def testParentMapWithMenus {
    val k =
      ConcreteKConfig(
        CMenu(1, Prompt(ROOT_NAME), List(
          CConfig(2, "A", cs = List(
            CMenu(3, Prompt("Doesn't Matter"), List(
              CConfig(4, "B", cs = List(
                CConfig(5, "C"))))))))))

    val pMap = toStringMap(mkConfigMap(k), k.allConfigs, "_ROOT")
    assert(pMap("A") == "_ROOT")
    assert(pMap("B") == "A")
    assert(pMap("C") == "B")
  }

}
