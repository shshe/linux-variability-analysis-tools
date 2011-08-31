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
import java.io.{FileWriter, OutputStreamWriter, PrintStream}

/**
 * Outputs a KExpr to text.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait ToDocument {
  def toDocument : Document

  def printGrammar(implicit out: PrintStream) : Unit = {
    val writer = new java.io.BufferedWriter(new OutputStreamWriter(out))
    this.toDocument.format(120, writer)
    writer write "\n"
    writer.flush
  }

  def writeGrammarToFile(file: String) : Unit = {
    val writer = new FileWriter(file)
    this.toDocument.format(120, writer)
    writer write "\n"
    writer.close
  }
}

trait KExprWriter {

  implicit def exprToGrammar(e : KExpr) = new KExprDoc(e)

  class KExprDoc(e : KExpr) extends ToDocument {
    def toDocument = e match {
      case And(l,r) => "(" :: l.toDocument :: " && " :: r.toDocument :: text(")")
      case Or (l,r) => "(" :: l.toDocument :: " || " :: r.toDocument :: text(")")
      case Eq (l,r) => "(" :: l.toDocument :: " = "  :: r.toDocument :: text(")")
      case NEq(l,r) => "(" :: l.toDocument :: " != " :: r.toDocument :: text(")")
      case NonCanonEq(l,r) => "(" :: l.toDocument :: " === " :: r.toDocument :: text(")")
      case Not(e)   => "!" :: e.toDocument
      case Group(id, e) => "[" :: text(id.toString) :: "-" :: e.toDocument :: text("]")
      case Literal(value) => "\"" :: text(value) :: text("\"")
      case Id(id) => text(id)
      case KInt(v) => text("" + v)
      case KHex(v) => text(v)
      case Yes => text("y")
      case No => text("n")
      case Mod => text("m")
    }
  }


}