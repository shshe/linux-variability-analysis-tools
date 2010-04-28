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

class KExprTest extends AssertionsForJUnit {

  import KExprParser.{parseKExpr => p}

  implicit def toId(s : String) = Id(s)

  @Test def literals() {
    assert(p("0x1") == KHex("0x1"))
    assert(p("1") == KInt(1))
    assert(p("A") == Id("A"))
    assert(p("y") == Yes)
    assert(p("n") == No)
    assert(p("m") == Mod)
  }

  @Test def equality() {
    assert(p("0x1 = 1") ==  Eq(KHex("0x1"),KInt(1)))
    assert(p("0x1 != 1") == NEq(KHex("0x1"),KInt(1)))
  }

  @Test def precedence() {
    assert(p("A || B && C") == Or("A", And("B", "C")))
    assert(p("A || B = C") == Or("A", Eq("B", "C")))
  }

  @Test def identifiers() {
    assert(p("64BIT") == Id("64BIT"))
  }

  @Test def hex() {
    assert(p("X=0xB000000") == Eq(Id("X"), KHex("0xB000000")))
  }
}