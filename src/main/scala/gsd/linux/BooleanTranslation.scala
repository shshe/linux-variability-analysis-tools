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
 * Builds the Boolean formula for an abstract Kconfig model.
 *
 * The translation is (or at least tries to be) a less constrained version of the
 * true Linux Kconfig semantics. The idea is that if inputted into a SAT solver,
 * any valid configuration of Linux will be satisfiable with the generated
 * formula. However, the inverse is not true; Some formulas that are satisfiable
 * with the generated formula may not be a valid configuration.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait BooleanTranslation extends KExprList with BExprList with BooleanRewriter {

  case class BTrans(exprs: List[BExpr], genVars: List[String])


  /**
   * Translates a KExpr to a propositional BExpr. This is a total mapping
   * from KExpr to BExpr.
   */
  def toBExpr(in: KExpr) = {
    def translate(e: KExpr): BExpr = e match {
      case No => False
      case Mod | Yes => True

      case Literal(_) | KHex(_) | KInt(_) => False

      case Eq(Id(n), Yes) => BId(n)
      case Eq(Id(n), Mod) => BId(n)
      case Eq(Id(n), No) => BNot(BId(n))
      case Eq(Id(n), Literal("")) => BNot(BId(n))
      case Eq(Id(n), Literal(_)) => BId(n)
      case Eq(Id(n), KHex(_)) => BId(n)
      case Eq(Id(n), KInt(_)) => BId(n)
      case Eq(Id(x), Id(y)) => BIff(BId(x), BId(y))

      case NEq(Id(n), Yes) => True //TODO Check this
      case NEq(Id(n), Mod) => True //TODO Check this
      case NEq(x, y) => BNot(translate(Eq(x, y)))

      case And(x, y) => BAnd(translate(x), translate(y))
      case Or(x, y) => BOr(translate(x), translate(y))

      case Not(e) => BNot(translate(e))
      case Id(n) => BId(n)

      case e => error("Unexpected: " + e + ": " + e.getClass)
    }
    translate(in)
  }

  /**
   * Factors out common sub-expressions from a list of disjunctions. This is
   * not used in the current implementation.
   *
   * FIXME ends up with a recursive loop
   */
  def factor(in: List[BExpr]): BExpr = {
    def _fact(disjs: List[BExpr]) : List[BExpr] = {
      val disjsLists = disjs.map { _.splitConjunctions }
      val conjs = disjsLists.flatten[BExpr]

      //map conjunct -> matching revLists pairs
      val occurs = conjs.map { c => c -> disjsLists.filter { _ contains c } }
              .filter { case (_,lst) => lst.size > 1 }

      if (occurs.isEmpty) disjs
      else {
        val (fact, contains) = occurs.reduceLeft{ (x,y) =>
          if (x._2.size > y._2.size) x
          else y
        }

        val factored = contains.map { _ - fact }.map { _.mkConjunction }
        val remaining = disjs -- contains.map { _.mkConjunction }
        (fact && _fact(factored).mkDisjunction) :: remaining
      }
    }
    _fact(in).mkDisjunction
  }


  def mkPresence(k: AbstractKConfig): BTrans = {

    /**
     * A container for a list of expressions and a list of generated
     * equivalences between the original expression and a generated variable.
     */
    case class EquivExprs(exprs: List[BExpr], equivs: List[BExpr])

    object IdGen {
      var i = 0
      def allIds = (1 to i).map { "x" + _ }.toList
      def next = { i+=1; "x" + i }
    }

    /**
     * Construct a list containing first i elements, the ith element is the
     * current default being processed, the 0 to i-1 elements are the previous
     * default conditions. Negate all previous default conditions and conjoin it
     * with the current default condition.
     */
    def mkDefaults(defs: List[Default]): List[BExpr] = {

      /**
       * Chunks defaults such that each list of defaults has the same consecutive
       * default values (ie. Default.iv)
       */
      def chunkDefaults(defs: List[Default]): List[List[Default]] = defs match {
        case Nil => Nil
        case Default(iv, _) :: _ =>
          val (same, rest) = defs.partition {_.iv == iv}
          same :: chunkDefaults(rest)
      }

      def negateDefaults(prevs: List[Default]) =
        ((True: BExpr) /: prevs) {(x, y) => x && !toBExpr(y.cond)}

      /**
       * Chunk defaults and reverse such that the last default chunk is first.
       * This is necessary since doChunks negates the tail, which is the first n-1
       * defaults when the list is reversed.
       *
       * For example, given defaults [X,Y,Z], the reverse is [Z,Y,X]. doChunks will
       * process Z first, with [Y,X] as the tail. Z is the last default, and thus,
       * must negate Y and X.
       */
      def doChunks(prevs: List[List[Default]])(acc: List[BExpr]): List[BExpr] =
        prevs match {
          case Nil => acc
          case lst :: rest =>
            val negatedPrev = negateDefaults(rest.flatten[Default])
            doChunks(rest) {

              //Remove defaults with value 'No' or Literal("")
              lst.remove { case Default(iv, _) => iv == No || iv == Literal("") }
                .map {
                  case Default(Yes, c) => negatedPrev && toBExpr(c)
                  case Default(Mod, c) => negatedPrev && toBExpr(c)
                  case Default(v: Value, c)   => negatedPrev && toBExpr(c)
                  case Default(iv, c) => negatedPrev && toBExpr(c && iv)
                } ++ acc
            }
        }

      doChunks(chunkDefaults(defs).reverse)(Nil)
    }

    def isTooConstraining(e: BExpr): Boolean =
      KExprRewriter.count { case _ : Eq | _ : NEq => 1}(e) > 0

    /**
     * Map from a sub-expression to a generated variable
     */
    val cache = new collection.mutable.HashMap[Set[BExpr], BId] {
      def apply(e: BExpr) =
        getOrElseUpdate(Set() ++ e.splitConjunctions, BId(IdGen.next))
    }

    /**
     * Replaces expressions with cached identifiers.
     */
    def replaceWithVars(lst: List[BExpr]): List[BExpr] = lst.map(cache.apply)

    /*
     * For each config, generate the presence expression and any equivalence
     * expressions generated by any new variables from the reverse dependencies.
     *
     */
    val exprs = k.configs.flatMap {
      case AConfig(id, _, _, pro, defs, krevs, _) =>
        val proE     = toBExpr(pro)
        val revDepE  = krevs.map(toBExpr) ::: mkDefaults(defs)
        val numOfIds = (0 /: (proE :: revDepE))( (x,y) => x + identifiers(y).size)

        //Heuristic for estimating size of resulting CNF translation
        if (numOfIds < 40 && revDepE.size < 5) {
          val ante = revDepE.remove(isTooConstraining).mkDisjunction
          val conseq = revDepE.mkDisjunction
          (proE || (ante implies BId(id)) && (BId(id) implies conseq)) :: Nil
        }
        else {
          val ante = replaceWithVars(revDepE.remove(isTooConstraining)).mkDisjunction
          val conseq = replaceWithVars(revDepE).mkDisjunction
          val equivs = revDepE.map { e => cache(e) iff factor(e.splitDisjunctions) }
          (proE || (ante implies BId(id)) && (BId(id) implies conseq)) :: equivs
        }
    }

    BTrans(exprs, IdGen.allIds)
  }

  def mkInherited(k: AbstractKConfig): List[BExpr] = k.configs.map {
    case AConfig(id, t, vis, pro, defs, rev, rngs) => BId(id) implies toBExpr(vis)
  }

  def mkChoice(k: AbstractKConfig): List[BExpr] = k.choices.flatMap {
    case AChoice(vis, isBool, isMand, memIds) =>
      val xors = Combinations.choose(2, memIds).map {
        case fst :: snd :: Nil => !BId(fst) || !BId(snd)
        case _ => error("This should never happen")
      }

      val disj = toBExpr(vis) implies (memIds.map(BId): List[BExpr]).reduceLeft(BOr)
      
      if (isMand) disj :: xors else xors
  }

  /**
   * Creates a BTrans for an AbstractKConfig.
   * BTrans is simply a wrapper for a boolean expression (BExpr) and a set of
   * generated variables.
   */
  def mkBooleanTranslation(k: AbstractKConfig) : BTrans = {
    val pres = mkPresence(k)
    BTrans(pres.exprs ::: mkInherited(k) ::: mkChoice(k), pres.genVars)
  }
}

trait BooleanRewriter extends Rewriter {
  def identifiers(e: BExpr): Set[String] = collects {
    case BId(n) => n
  }(e)
}

object BooleanTranslation extends BooleanTranslation