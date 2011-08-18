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

package gsd.linux.stats

import org.kiama.rewriting.Rewriter._
import gsd.linux._

import TypeFilterList._

/**
 * A collection of useful statistics on the Concrete Kconfig model.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
class FeatureStatistics(val k: ConcreteKConfig) {

  lazy val configs = collectl {
    case c: CConfig if !c.isMenuConfig => c
  }(k)

  lazy val menuconfigs = collectl {
    case c: CConfig if  c.isMenuConfig => c
  }(k)

  lazy val menus = collectl {
    case m: CMenu => m
  }(k)

  lazy val choices = collectl {
    case c: CChoice => c
  }(k)

  lazy val allConfigs = collectl {
    case c: CConfig => c
  }(k)

  lazy val features = configs ++ menuconfigs ++ menus ++ choices

  lazy val boolConfigs   = allConfigs.filter { _.ktype == KBoolType }
  lazy val triConfigs    = allConfigs.filter { _.ktype == KTriType }
  lazy val stringConfigs = allConfigs.filter { _.ktype == KStringType }
  lazy val intConfigs     = allConfigs.filter { _.ktype == KIntType }
  lazy val hexConfigs     = allConfigs.filter { _.ktype == KHexType }

  lazy val promptConfigs    = allConfigs.filter { _.prompt.size > 0 }
  lazy val nonPromptConfigs = allConfigs filterNot (promptConfigs contains)

  lazy val mandChoice = choices.filter { _.isMand }
  lazy val optChoice = choices.filter { !_.isMand }
  lazy val boolChoice = choices.filter { _.isBool }
  lazy val triChoice = choices.filter { !_.isBool }

  lazy val groupedMap = Map() ++ choices.map { c => (c, c.children) }
  lazy val grouped = groupedMap.flatMap { case (k,v) => v }.toList.typeFilter[CConfig]
  lazy val groupedBool = grouped.filter { _.ktype == KBoolType }
  lazy val groupedTri  = grouped.filter { _.ktype == KTriType }

  lazy val xorGroups = choices.filter { c => c.isBool && c.isMand }
  lazy val orGroups = choices.filter { c => !c.isBool && c.isMand }
  lazy val mutexGroups = choices.filter { c => c.isBool && !c.isMand }
  lazy val optGroups = choices.filter { c => !c.isBool && !c.isMand } // for completeness

  lazy val leafDepthMap : Map[CConfig, Int] = {
    def addChildren(depth: Int)(elem: CSymbol) : List[(CConfig,Int)] = elem match {
      case c: CConfig if c.children.isEmpty =>
        List((c, depth))
      case x if x.isVirtual =>
        elem.children.flatMap { addChildren(depth) }
      case _ =>
        elem.children.flatMap { addChildren(depth+1) }
    }
    addChildren(0)(k.root) toMap
  }

  lazy val branchingMap : Map[CSymbol,List[CSymbol]] =
    features map { f => (f, f.children) } toMap

  lazy val properties = collectl {
    case CConfig(_,_,_,_,_,pro,defs,sels,rngs,_,_) =>
      pro.toList ::: defs ::: sels ::: rngs
    case CMenu(_,pro,_) => pro
    case CChoice(_,pro,_,_,defs,_) => pro :: defs
  }(k)

  lazy val ranges   = properties.typeFilter[Range]
  lazy val prompts  = properties.typeFilter[Prompt]
  lazy val selects  = properties.typeFilter[Select]
  lazy val defaults = properties.typeFilter[Default]

  

}
