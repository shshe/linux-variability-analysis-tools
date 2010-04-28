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
 * Methods for analyzing the hierarchy of a concrete Kconfig model.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait Hierarchy {

  def mkParentMap(k: ConcreteKConfig): Map[CConfig, Option[CConfig]] = {
    def _mkTuples(par: Option[CConfig])(curr: CSymbol): List[Pair[CConfig, Option[CConfig]]] =
      curr match {
        case c: CConfig => (c, par) :: c.children.flatMap { _mkTuples(Some(c)) }
        case m: CMenu => m.children.flatMap { _mkTuples(par) }
        case c: CChoice => c.children.flatMap { _mkTuples(par) }
      }
    Map() ++ _mkTuples(None)(k.root)
  }

  def toIdMap(in: Map[CConfig, Option[CConfig]], root: String) =
    Map() ++ in.map {
      case (x,Some(y)) => x.id -> y.id
      case (x,None) => x.id -> root
    }


}