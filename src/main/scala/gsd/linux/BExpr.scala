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
 * A set of classes for representing Boolean expressions.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
sealed abstract class BExpr {
  def ||(other: BExpr) = BOr(this, other)
  def &&(other: BExpr) = BAnd(this, other)
  def implies(other: BExpr) = BImplies(this, other)
  def iff(other: BExpr) = BIff(this, other)
  def unary_! = BNot(this)

  def splitConjunctions : List[BExpr] = this match {
    case BAnd(x,y) => x.splitConjunctions ::: y.splitConjunctions
    case e => List(e)
  }
  def splitDisjunctions : List[BExpr] = this match {
    case BOr(x,y) => x.splitDisjunctions ::: y.splitDisjunctions
    case e => List(e)
  }
}

sealed abstract class BBinaryOp(left: BExpr, right: BExpr, op: String) extends BExpr {
  override def toString = "(" + left + " " + op + " " + right + ")"
}

case class BAnd(l: BExpr, r: BExpr) extends BBinaryOp(l,r,"&&")
case class BOr(l: BExpr, r: BExpr) extends BBinaryOp(l,r,"||")
case class BIff(l: BExpr, r: BExpr) extends BBinaryOp(l,r,"<->")
case class BImplies(l: BExpr, r: BExpr) extends BBinaryOp(l,r,"->")

case object True extends BExpr {
  override def toString = "1"
}
case object False extends BExpr {
  override def toString = "0"
}

case class BNot(e: BExpr) extends BExpr {
  override def toString = "!" + e
}
case class BId(value: String) extends BExpr {
  override def toString = value
}
