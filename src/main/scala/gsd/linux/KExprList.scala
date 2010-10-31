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

/**
 * Convenience methods for lists containing KExprs.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait KExprList {

  implicit def toExpressionList(lst : List[KExpr]) =
    new RichExpressionList(lst)

  class RichExpressionList(lst : List[KExpr]) {
    def mkDisjunction = lst match {
      case Nil => No
      case _ => lst.reduceLeft(_ || _)
    }
    def mkConjunction = lst match {
      case Nil => Yes
      case _ => lst.reduceLeft(_ && _)
    }
  }

}

object KExprList extends KExprList
