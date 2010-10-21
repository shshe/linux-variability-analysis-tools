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
 * Convenience methods for Lists containing BExprs.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait BExprList {

  implicit def toBExprList(lst : List[BExpr]) =
    new BExprList(lst)

  class BExprList(lst : List[BExpr]) {
    def mkDisjunction = lst match {
      case Nil => BFalse
      case _ => lst.reduceLeft(_ | _)
    }
    def mkConjunction = lst match {
      case Nil => BTrue
      case _ => lst.reduceLeft(_ & _)
    }
  }

}