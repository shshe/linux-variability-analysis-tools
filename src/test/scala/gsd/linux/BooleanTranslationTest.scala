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
import org.junit.{Ignore, Test}

import BooleanTranslation._

class BooleanTranslationTest extends AssertionsForJUnit {
  
  implicit def toId(s: String) = Id(s)

  def b(k: ConcreteKConfig) =
    mkBooleanTranslation(k.toAbstractKConfig)

  @Test def rewriteKExpr {
    assert(toBExpr(Eq("A",Literal(""))) == BNot(BId("A")))
    assert(toBExpr(Eq("A",Literal("xyz"))) == BId("A"))
    assert(toBExpr(Eq("A",Literal("")) && Eq("B",Literal("xyz"))) == BAnd(BNot(BId("A")), BId("B")))
  }

  @Test def testIsTooConstrainingKExpr {
    assert(isTooConstraining(Eq("A","B")))
    assert(isTooConstraining(NEq("A","B")))
    assert(isTooConstraining(NEq("A","B") || "A" || "B"))
    assert(!isTooConstraining("A" || "B"))
  }

  @Test def testIsTooConstrainingDefault {
    assert(isTooConstraining(ADefault("A", Nil, Eq("B", "C"))))
    assert(isTooConstraining(ADefault("A", Nil, NEq("C", "F") || "G")))
    assert(isTooConstraining(ADefault(Eq("A","B"), Nil, "C")))
    assert(!isTooConstraining(ADefault("A", Nil, "B" || "C")))
  }

}