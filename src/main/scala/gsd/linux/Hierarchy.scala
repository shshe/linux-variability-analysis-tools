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
object Hierarchy {

  type CParentMap = Map[CConfig, CConfig]
  type AParentMap = Map[CSymbol, CConfig]
  type HierarchyMap = Map[CSymbol, CSymbol]

  /**
   * Creates a map containing only of concrete features: Configs and not Menus
   * or Choices.
   */
  def mkConfigMap(k: ConcreteKConfig): CParentMap =
    (mkParentMap(k) collect {
      case x@(_:CConfig,_) => x
    }).asInstanceOf[Map[CConfig, CConfig]]

  /**
   * A map from any feature (config, menu and choices) to its closest Config.
   * Features that have no config in its ancestors are not present in the
   * returned map.
   * 
   * This map contains all features - configs, menus and choices.
   *
   * FIXME If nodes.
   */
  def mkParentMap(k: ConcreteKConfig): AParentMap = {

    def _mkTuples(par: Option[CConfig])(curr: CSymbol): List[(CSymbol, CConfig)] =
      par match {

        case None => curr match {
          case c:CConfig => curr.children.flatMap(_mkTuples(Some(c)))
          case _ => curr.children.flatMap(_mkTuples(None))
        }

        //A parent exists - curr's ancestors contains a config.
        case Some(p) => curr match {
          case c: CConfig =>
            (c, p) :: c.children.flatMap(_mkTuples(Some(c)))
          case _ =>
            (curr, p) :: curr.children.flatMap(_mkTuples(par))
        }
      }

    Map() ++ _mkTuples(None)(k.root)
  }

  /**
   * A map from a feature to its immediate parent.
   */
  def mkHierarchyMap(k: ConcreteKConfig): HierarchyMap = {
    def _mkTuples(p: CSymbol)(c: CSymbol): List[(CSymbol, CSymbol)] =
      (c, p) :: c.children.flatMap(_mkTuples(c))
    Map() ++ k.root.children.flatMap(_mkTuples(k.root))
  }


  def toStringMap(in: Map[CConfig, CConfig], features: Iterable[CConfig], root: String) =
    features map
      { f => f -> in.get(f) } map
      {
        case (x, Some(y)) => x.name -> y.name
        case (x, None) => x.name -> root
      } toMap


}