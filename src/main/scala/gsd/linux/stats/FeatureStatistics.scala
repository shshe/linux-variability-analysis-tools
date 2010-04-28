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

import kiama.rewriting.Rewriter
import gsd.linux._

/**
 * A collection of useful statistics on the Concrete Kconfig model.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait FeatureStatistics extends Rewriter with TypeFilterList {

  val k : ConcreteKConfig

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

  lazy val allConfigs = configs ++ menuconfigs

  lazy val features = configs ++ menuconfigs ++ menus ++ choices

  lazy val boolConfigs   = allConfigs.filter { _.ktype == KBoolType }
  lazy val triConfigs    = allConfigs.filter { _.ktype == KTriType }
  lazy val stringConfigs = allConfigs.filter { _.ktype == KStringType }
  lazy val intConfig     = allConfigs.filter { _.ktype == KIntType }
  lazy val hexConfig     = allConfigs.filter { _.ktype == KHexType }

  lazy val promptConfigs    = allConfigs.filter { _.prompt.isDefined }
  lazy val nonPromptConfigs = allConfigs -- promptConfigs

  lazy val mandChoice = choices.filter { _.isMand }
  lazy val optChoice = choices.filter { !_.isMand }
  lazy val boolChoice = choices.filter { _.isBool }
  lazy val triChoice = choices.filter { !_.isBool }

  lazy val groupedMap = Map() ++ choices.map { c => (c, c.children) }
  lazy val grouped = groupedMap.flatMap { case (k,v) => v }.toList.typeFilter[CConfig]
  lazy val groupedBool = grouped.filter { _.ktype == KBoolType }
  lazy val groupedTri  = grouped.filter { _.ktype == KTriType }

  lazy val xorGroups = choices.filter { _.isBool }
  lazy val orGroups = choices.filter { !_.isBool }
  lazy val mutexGroups = choices.filter { c => c.isBool && !c.isMand }

  lazy val mandMenus = menus.filter { _.prompt.cond != Yes }
  
  lazy val leafDepthMap : Map[CSymbol, Int] = {
    def addChildren(depth: Int)(elem: CSymbol) : List[Tuple2[CSymbol,Int]] =
      if (elem.children.isEmpty)
        (elem, depth) :: elem.children.flatMap { addChildren(depth+1) }
      else elem.children.flatMap { addChildren(depth+1) }

    Map() ++ addChildren(0)(k.root)
  }

  lazy val branchingMap : Map[CSymbol,List[CSymbol]] =
    Map() ++ features.map { f => (f, f.children) }

  lazy val parentTuples = collectl {
    case c:CSymbol => c.children.map { _.id -> c.id }
  }(k).flatten[Tuple2[String,String]]
  
  lazy val parentMap = Map() ++ parentTuples

  lazy val identifiers = collectl {
    case c:CSymbol => c.id
  }(k)

  lazy val properties = collectl {
    case CConfig(id,_,_,_,pro,defs,sels,rngs,_,_) =>
      pro.toList ::: defs ::: sels ::: rngs
    case CMenu(pro,_) => pro
    case CChoice(pro,_,_,defs,_) => pro :: defs
  }(k)

  lazy val ranges   = properties.typeFilter[Range]
  lazy val prompts  = properties.typeFilter[Prompt]
  lazy val selects  = properties.typeFilter[Select]
  lazy val defaults = properties.typeFilter[Default]

  

}
