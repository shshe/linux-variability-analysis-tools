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

import text._
import text.Document._
import java.io.{OutputStreamWriter, PrintStream}

/**
 * A VERY rough implementation to output the abstract syntax of a Kconfig extract
 * to text.
 *
 * @author Steven She (shshe@uwaterloo.ca)
 */
trait AbstractSyntaxWriter extends KExprWriter {
  trait ToDoc { def toDocument : Document }

  implicit def toExprDoc(e: KExpr) = e.toDocument
  implicit def toRangedoc(r: Range) = new RangeDoc(r)
  implicit def toDefaultDoc(d: Default) = new DefaultDoc(d)
  implicit def toConfigDoc(c: AConfig) = new ConfigDoc(c)
  implicit def toAbstractKConfigDoc(k: AbstractKConfig) = new AbstractKConfigDoc(k)

  implicit val out = System.out

  class AbstractKConfigDoc(k: AbstractKConfig) extends ToDoc {
    def printGrammar(implicit out: PrintStream) : Unit = {
      val writer = new java.io.BufferedWriter(new OutputStreamWriter(out))
      toDocument.format(120, writer)
      writer write "\n"
      writer.flush
    }
    def toDocument = ((empty : Document) /: k.configs.map { _.toDocument })(_ :/: _)
  }

  class ConfigDoc(c: AConfig) extends ToDoc {
    def toDocument = {
      ("config " + c.id + " " + typeString(c.ktype) + " {") :/:
        group {
          "prompt [" :: c.pro :: "]\n" ::
          "rev [" :: ((empty : Document) /: c.rev.map { _.toDocument })(_ :/: _) :: "]" :: text("\n") ::
          group(((empty : Document) /: c.defs.map { _.toDocument })(_ :/:_)) :/:
          group(((empty : Document) /: c.ranges.map { _.toDocument })(_ :/:_))
        } :/:
      text("}")
    }
  }

  class DefaultDoc(d: Default) extends ToDoc {
    def toDocument = "default [" :: d.iv :: "] if [" :: d.cond :: text("]")
  }

  class RangeDoc(r: Range) extends ToDoc {
    def toDocument = "range [" :: r.low :: " " :: r.high :: "] if [" :: r.cond :: text("]")
  }

  def typeString(t: KType) = t match {
    case KBoolType => "boolean"
    case KTriType  => "tristate"
    case KStringType => "string"
    case KHexType => "hex"
    case KIntType => "int"
  }
}