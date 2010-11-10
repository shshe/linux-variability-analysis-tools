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

import scala.text._
import scala.text.Document._

/**
 * Outputs a concrete Kconfig model to text.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait KConfigWriter extends KExprWriter {

  implicit def elementToDoc(sym: CSymbol) = new SymbolDoc(sym)
  implicit def configTypeToDoc(t: KType) = new KTypeDoc(t)
  implicit def propertyToDoc(p: Property) = new PropertyDoc(p)
  implicit def kConfigToDoc(k: ConcreteKConfig) = new KConfigDoc(k)

  implicit val out = System.out

  class KConfigDoc(k: ConcreteKConfig) extends ToDocument {
    def toDocument = new SymbolDoc(k.root).toDocument
    

  }

  class SymbolDoc(sym: CSymbol) extends ToDocument {

    def mkChildren(cs: List[CSymbol]) : Document =
      cs.map { _.toDocument }.foldLeft(empty : Document){_ :/: _ }

    def mkChoiceType(isBool: Boolean) = if (isBool) text("boolean") else text("tristate")
    def mkMand(o: Boolean) = if (!o) text("optional") else empty

    def mkPrompt(p: Prompt) = p match {
      case Prompt(txt,cond) =>
        group("prompt" :/: "\"" + txt + "\"" :/: "[" :: cond.toDocument :: text("]"))
    }

    def mkInherited(e: KExpr) = group("inherited [" :: e.toDocument :: text("]"))

    def toDocument = sym match {

      case CConfig(id,isMenu,t,inh,pro,defs,sels,rngs,deps,cs) =>
        group((if (isMenu) "config" else "menuconfig") :/: id :/: t.toDocument :/: text("{")) :/:
          nest(2, pro.foldLeft(empty:Document) { (x,y) => x :/: mkPrompt(y) })  :/:
          nest(2, (defs ::: sels ::: rngs).foldRight(empty: Document){_.toDocument :/: _}) :/:
          nest(2, mkInherited(inh)) :/:
          nest(2, mkChildren(cs)) :/: text("}")

      case CMenu(Prompt(txt,cond),cs) =>
        group("menu" :/: "\"" + txt + "\"" :/: text("{")) :/:
          nest(2, group("depends on" :/: "[" :: cond.toDocument :: text("]"))) :/:
          nest(2, mkChildren(cs)) :/: text("}")
      
      case CChoice(pro,isBool,isMand,defs,cs) =>
        group("choice" :/: mkChoiceType(isBool) :/: mkMand(isMand) :/: text("{")) :/:
          nest(2, mkPrompt(pro)) :/:
          nest(2, defs.foldRight(empty: Document){_.toDocument :/: _}) :/:
          nest(2, mkChildren(cs)) :/: text("}")
    }
  }

  class KTypeDoc(ctype: KType) extends ToDocument {
    def toDocument = text(ctype match {
      case KBoolType => "boolean"
      case KTriType => "tristate"
      case KIntType => "int"
      case KHexType => "hex"
      case KStringType => "string"
    })
  }

  class PropertyDoc(p: Property) extends ToDocument {
    def toDocument = group(p match {
      case Prompt(txt,vis) =>
        "prompt" :/: txt :/: "if " :: "[" :: vis.toDocument :: text("]")
      case Default(expr,vis) =>
        "default" :/: expr.toDocument :/: "if " :: "[" :: vis.toDocument :: text("]")
      case Select(target,vis) =>
        "select" :/: target :/: "if " :: "[" :: vis.toDocument :: text("]")
      case Range(low,high,vis) =>
        "range" :/: "[" :: low.toDocument :: " " :: high.toDocument :: "]" :/:
        "if " :: "[" :: vis.toDocument :: text("]")
      case Env(id,vis) =>
        "env" :/: id :/: "if " :/: "[" :: vis.toDocument :: text("]")
    })
  }
}

