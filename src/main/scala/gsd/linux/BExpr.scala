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

trait Expr

object BExpr {
  implicit def toB2ExprList(lst: List[BExpr]) = new B2ExprList(lst)

  class B2ExprList(lst: List[BExpr]) {
    def ||(): BExpr = ((BFalse: BExpr) /: lst){ _ | _ }
  }
}
sealed abstract class BExpr extends Expr {
  def |(o: BExpr): BExpr = BOr(this, o)
  def &(o: BExpr): BExpr = BAnd(this, o)
  def iff(o: BExpr): BExpr = BIff(this, o)
  def implies(o: BExpr): BExpr = BImplies(this, o)

  def unary_!(): BExpr = BNot(this)

  def splitConjunctions(): List[BExpr] = this match {
    case BAnd(x,y) => x.splitConjunctions ::: y.splitConjunctions
    case e => List(e)
  }
  def splitDisjunctions(): List[BExpr] = this match {
    case BOr(x,y) => x.splitDisjunctions ::: y.splitDisjunctions
    case e => List(e)
  }

  lazy val simplify: BExpr = this
}

trait BinarySimplify extends BExpr {
  val l, r: BExpr
  def simp(f: (BExpr, BExpr) => BExpr) = f(l.simplify, r.simplify)
}

case class BNot(e: BExpr) extends BExpr {

  override def toString = "!" + e

  override lazy val simplify = e match {
    case BNot(f) => f.simplify
    case BTrue => BFalse
    case BFalse => BTrue
    case _ => BNot(e.simplify)
  }
}

case class BAnd(l: BExpr, r: BExpr) extends BExpr with BinarySimplify {

  override def toString = "(" + l + " & " + r + ")"

  override lazy val simplify =
    if (l == BFalse || r == BFalse) BFalse
    else simp(BAnd)
}

case class BOr(l: BExpr, r: BExpr) extends BExpr with BinarySimplify {

  override def toString = "(" + l + " | " + r + ")"

  override lazy val simplify =
    if (l == BTrue || r == BTrue) BTrue
    else simp(BOr)

}

case class BIff(l: BExpr, r: BExpr) extends BExpr with BinarySimplify {
  override def toString = "(" + l + " <=> " + r + ")"

  override lazy val simplify = (l.simplify, r.simplify) match {
    case (BTrue, _) => r.simplify
    case (_, BTrue) => l.simplify
    case (BFalse,_) => ((!r).simplify)
    case (_,BFalse) => ((!l).simplify)
    case _ => simp(BIff)
  }
}

case class BImplies(l: BExpr, r: BExpr) extends BExpr with BinarySimplify {
  override def toString = "(" + l + " -> " + r + ")"
  override lazy val simplify = simp(BImplies)
}

case class BId(v: String) extends BExpr {
  override def toString = v
}
case object BTrue extends BExpr {
  override def &(o: BExpr) = o
  override def toString = "1"
}
case object BFalse extends BExpr {
  override def |(o: BExpr) = o
  override def toString = "0"
}

