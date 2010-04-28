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
 * The extract used by LVAT is an intermediate format of the Kconfig language
 * used by the Linux kernel. This intermediate format has been post processed
 * by the kernel infrastructure. As a result, the conditions on the properties
 * are a conjunction of the conditions declared on the properties themselves and
 * any inherited conditions (i.e. from any ancestor menus or choices).
 *
 * This object provides methods to remove these inherited conditions from the
 * properties.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object ConstraintRewriter extends Rewriter with KExprList {

    def rewriteExpr(in: KExpr)(e: KExpr) = {
      (e.splitConjunctions -- in.splitConjunctions).mkConjunction
    }

    def sRemoveInherited(in: KExpr) = everywheretd {
      rule {
        case Default(iv, cond) => Default(iv, rewriteExpr(in)(cond))
        case Prompt(text,cond) => Prompt(text, rewriteExpr(in)(cond))
        case Select(id,cond)   => Select(id, rewriteExpr(in)(cond))
        case Range(low,high,cond) => Range(low, high, rewriteExpr(in)(cond))
        case Env(id,cond) => Env(id, rewriteExpr(in)(cond))
      }
    }

    def traverseSymbol(c: CSymbol): CSymbol = {
      c match {
        case CConfig(id,isMenu,t,in,pro,defs,sels,rngs,deps,cs) =>
          CConfig(id,isMenu,t,in,
            rewrite(sRemoveInherited(in))(pro),
            rewrite(sRemoveInherited(in))(defs),
            rewrite(sRemoveInherited(in))(sels),
            rewrite(sRemoveInherited(in))(rngs),
            deps,
            cs.map { traverseSymbol })
        case CMenu(pro,cs) => CMenu(pro, cs.map { traverseSymbol })
        case CChoice(pro,isBool,isMand,defs,cs) =>
          CChoice(pro, isBool, isMand, defs, cs.map { traverseSymbol })
      }
    }

    def rewriteInherited(k: ConcreteKConfig) : ConcreteKConfig =
      ConcreteKConfig(traverseSymbol(k.root).asInstanceOf[CMenu])
}

