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

/**
 * A set of classes for representing expressions in the Kconfig language.
 * Supports its tristate logic, equality and inequalities in expressions.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
sealed abstract class KExpr(children : List[KExpr]) {

  def ||(other : KExpr) : KExpr = Or(this,other)
  def &&(other : KExpr) : KExpr = And(this, other)
  def unary_! : KExpr = Not(this)

  def splitConjunctions : List[KExpr] = this match {
    case And(x,y) => x.splitConjunctions ::: y.splitConjunctions
    case e => List(e)
  }
  def splitDisjunctions : List[KExpr] = this match {
    case Or(x,y) => x.splitDisjunctions ::: y.splitDisjunctions
    case e => List(e)
  }

}

object KExprRewriter extends Rewriter {}

sealed abstract class BinaryOp(l : KExpr, r : KExpr, opStr : String) extends KExpr(List(l,r)) {
  override def toString = "(" + l + " " + opStr + " " + r + ")"
}

sealed abstract class UnaryOp(e : KExpr, opStr : String) extends KExpr(List(e)) {
  override def toString = opStr + e
}

case class And(l: KExpr, r: KExpr) extends BinaryOp(l, r, "&&")
case class Or (l: KExpr, r: KExpr) extends BinaryOp(l, r, "||")
case class Eq (l: IdOrValue, r: IdOrValue) extends BinaryOp(l, r, "=")
case class NEq(l: IdOrValue, r: IdOrValue) extends BinaryOp(l, r, "!=")
case class Not(e: KExpr) extends UnaryOp(e, "!")

sealed abstract class IdOrValue extends KExpr(Nil)
sealed abstract class Value extends IdOrValue
sealed abstract class Number extends Value
case class Id(value : String) extends IdOrValue
case class Literal(value : String) extends Value
case class KInt(value : Int) extends Number
case class KHex(value : String) extends Number
case object Yes extends Value
case object No  extends Value
case object Mod extends Value

