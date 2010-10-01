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

import kiama.rewriting.Rewriter

case class ConcreteKConfig(root: CMenu) {
  val rw = new Object with Rewriter

  lazy val plainConfigs: List[CConfig] = rw.collectl {
    case c: CConfig if !c.isMenuConfig => c
  }(root)

  lazy val menuConfigs: List[CConfig] = rw.collectl {
    case c: CConfig if  c.isMenuConfig => c
  }(root)

  lazy val allConfigs: List[CConfig] = plainConfigs ++ menuConfigs

  lazy val choices: List[CChoice] = rw.collectl {
    case c: CChoice => c
  }(root)

  lazy val menus: List[CMenu] = rw.collectl {
    case m: CMenu => m
  }(root)

  lazy val features: List[CSymbol] =
    allConfigs ++ menus ++ choices

  /**
   * Set of all identifiers and identifier references in the Kconfig model.
   */
  lazy val identifiers = rw.collects {
    case c: CConfig => c.id
    case Id(n) => n
  }(root)


  def toAbstractKConfig =
    new AbstractSyntaxBuilder(this).toAbstractSyntax
}

case class AbstractKConfig(configs: List[AConfig], choices: List[AChoice]) {
  /**
   * Maps an identifier to its abstract config
   */
  lazy val idMap: Map[String, AConfig] = Map() ++ configs.map {
    case c: AConfig => c.id -> c
  }
}

object AbstractKConfig {
  implicit def fromConcreteKConfig(k: ConcreteKConfig): AbstractKConfig =
    new AbstractSyntaxBuilder(k).toAbstractSyntax
}

sealed abstract class CSymbol(val id: String,
                              val properties: List[Property],
                              val children: List[CSymbol])
sealed abstract class ASymbol

/* ~~~~~~~~~~~~~~~
 * Abstract Syntax
 * ~~~~~~~~~~~~~~~ */
case class AConfig(id: String, ktype: KType, vis: KExpr, pro: KExpr,
                   defs: List[Default], rev: List[KExpr], ranges: List[Range])
        extends ASymbol

case class AChoice(vis: KExpr, isBool: Boolean, isMand: Boolean,
                   memIds: List[String])
        extends ASymbol

/* ~~~~~~~~~~~~~~~
 * Concrete Syntax
 * ~~~~~~~~~~~~~~~ */
case class CConfig(override val id: String,
                   isMenuConfig: Boolean,
                   ktype: KType,
                   inherited: KExpr,
                   prompt: Option[Prompt],
                   defs: List[Default], sels: List[Select], ranges: List[Range],
                   depends: List[DependsOn],
                   cs: List[CSymbol])
        extends CSymbol(id, prompt.toList ::: defs ::: sels ::: ranges, cs)

case class CMenu(prompt: Prompt, cs: List[CSymbol])
        extends CSymbol("\"" + prompt.text + "\"", List(prompt), cs)

case class CChoice(prompt: Prompt, isBool: Boolean, isMand:Boolean,
                   defs: List[Default], cs: List[CSymbol])
        extends CSymbol("\"" + prompt.text + "\"", prompt :: defs, cs)

sealed abstract class Property(val cond: KExpr)
case class Prompt(text: String, c: KExpr) extends Property(c)
case class Default(iv: KExpr, c: KExpr) extends Property(c)
case class Select(id: String, c: KExpr) extends Property(c)
case class Range(low: IdOrValue, high: IdOrValue, c: KExpr) extends Property(c)
case class Env(id: String, c: KExpr) extends Property(c)

case class DependsOn(cond: KExpr)

sealed abstract class KType
case object KBoolType extends KType
case object KTriType extends KType
case object KHexType extends KType
case object KIntType extends KType
case object KStringType extends KType
