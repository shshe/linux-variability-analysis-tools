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

import KConfigParser.mkConfig

trait KConfigBuilder {

  def mkMenu(text: String, cond: KExpr, cs: List[CSymbol]) =
    CMenu(Prompt(text,cond), false, cs)

  def mkBConfig(id: String, props: List[Property]): CConfig =
    mkConfig(id, false, KBoolType, props, Yes, Nil, Nil)

  def mkBConfig(id: String, props: List[Property], cs: List[CSymbol]): CConfig =
    mkConfig(id, false, KBoolType, props, Yes, Nil, cs)

  def mkRoot(syms: List[CSymbol]) =
    ConcreteKConfig(CMenu(Prompt("Linux Kernel Configuration", Yes), false, syms))


}