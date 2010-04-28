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

import KConfigParser._
import KExprList._
import stats.ConstraintRewriter

class AbstractSyntaxTest extends AssertionsForJUnit with KConfigBuilder
    with AbstractSyntax with AbstractSyntaxWriter {

  implicit def toRichList(lst: List[KExpr]) = new {
    def toSet: Set[KExpr] = Set() ++ lst
  }
  implicit def toId(s: String) = Id(s)

  @Test def rev() {
    val sels = Select("A", "X" || "Y") :: Select("A", "Z") :: Nil
    val configs = mkBConfig("A",Nil) :: mkBConfig("B",sels) :: Nil
    val root = mkRoot(configs)
    assert(root.rev("A").toSet == Set("B" && ("X" || "Y"),"B" && "Z"))
  }

  @Test def find {
    val root = mkRoot(mkBConfig("A",Nil) :: mkBConfig("B",Nil) :: Nil)
    assert(root.find("A").get.id == "A")
    assert(root.find("B").get.id == "B")
  }
}