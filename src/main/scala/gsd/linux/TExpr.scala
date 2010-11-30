package gsd.linux

import collection.mutable.ListBuffer
import util.logging.Logged

object TExpr extends Logged {

  /**
   * Limited functionality, used to convert reverse dependency expression.
   */
  def toTExpr(in: KExpr): TExpr = {
    def t(e: KExpr): TExpr = e match {
      case Id(n) => TId(n)

      case No  => TNo
      case Mod => TMod
      case Yes => TYes

      case And(x, y) => t(x) & t(y)
      case Or(x, y) => t(x) | t(y)
      case Eq(l, r) => t(l) teq t(r)
      case NEq(l, r) => !(t(l) teq t(r))
      case Not(e) => !t(e)

      case Literal("") => //FIXME ?
        TNo
      case Literal(_) | KHex(_) | KInt(_) =>
        log("WARN: Literal / Hex / Int not handled, returning TYes")
        TYes

      case e => error("Unexpected expression (is it a boolean op?): " + e + ": " + e.getClass)
    }
    t(in)
  }

}

abstract class TExpr {
  def toBExpr: (BExpr, BExpr)

  /**
   * Returns a single boolean expression.
   */
  def eq(other: TExpr): BExpr = other match {

    case TNo => toBExpr match {
      case (e1, e2) => !e1 & !e2
    }

    case TMod => toBExpr match {
      case (e1, e2) => e1 & !e2
    }

    case TYes => toBExpr match {
      case (e1, e2) => e1 & e2
    }

    case _ => (toBExpr, other.toBExpr) match {
      case ((l1, l2), (r1, r2)) => (l1 iff r1) & (l2 iff r2)
    }
  }

  /**
   * Returns a single boolean expression.
   */
  def <=(other: TExpr): BExpr = (this, other) match {
    
    case (TNo,_)  => BTrue
    case (TYes,_) => other eq TYes

    case (_,TYes) => BTrue
    case (_,TNo)  => this eq TNo

    case _ => (toBExpr, other.toBExpr) match {

        // x <= Mod
        case ((l1, l2), (BTrue,BFalse)) => !l2

        //Mod <= x
        case ((BTrue, BFalse),(r1, r2)) => r1

        case ((l1, l2), (r1, r2)) =>
          // Before simplifying:
          // (!(l1 | l2 ) implies (r1 | r2)) & !(l1 & l2 & r1 & !r2)
          (!l1 | r1 | r2) & (!l2 | r1 | r2 ) & (!l1 | !l2 | !r1 | r2)
      }
  }

  /**
   * Returns a single boolean expression.
   */
  def >(other: TExpr): BExpr = (this, other) match {
    
    case (TMod,_) => this eq TNo
    case (_,TMod) => this eq TYes

    case _ => (toBExpr, other.toBExpr) match {
        //Yes > x
        case ((BTrue, BTrue), (r1, r2)) => !r2

        //x > No
        case ((l1, l2), (BFalse, BFalse)) => l1

        //FIXME
        case ((l1, l2), (r1, r2)) =>
          l1 & !r2 & (!l1 | l2 | !r1 | r2)
      }
  }

  def teq(other: TExpr): TExpr =
    TEq(this, other)

  def &(other: TExpr): TExpr = TAnd(this, other)
  def |(other: TExpr): TExpr = TOr(this, other)
  def unary_! = TNot(this)
}

case object TYes extends TExpr {
  def toBExpr = (BTrue, BTrue) //(1,1)
}
case object TMod extends TExpr {
  def toBExpr = (BTrue, BFalse) //(1,0)
}
case object TNo extends TExpr {
  def toBExpr = (BFalse, BFalse) //(0,0)
}

case class TId(v: String) extends TExpr {
  def toBExpr = (BId(v + "_1"), BId(v + "_2"))
}

case class TAnd(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 & r1, l2 & r2)
  }
}

case class TOr(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 | r1, l2 | r2)
  }
}

case class TNot(e: TExpr) extends TExpr {
  def toBExpr = e.toBExpr match {
    case (l, r) => (!r, !l)
  }
}

case class TEq(left: TExpr, right: TExpr) extends TExpr {
  def toBExpr = (left.toBExpr, right.toBExpr) match {
    case ((l1, l2), (r1, r2)) => (l1 iff r1, l2 iff r2)
  }
}


