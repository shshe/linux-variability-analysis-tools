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

import collection.mutable.{MultiMap, HashMap}

import TypeFilterList._
import org.kiama.rewriting.Rewriter

/**
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object AbstractSyntax {
  implicit def toAbstractSyntaxBuilder(k: ConcreteKConfig) =
    new AbstractSyntaxBuilder(k)
  
  /**
   * Adds the base default to a list of defaults
   */
  def addBaseDefault(t: KType, defs: List[Default]) = {
    val baseDef = t match {
      case KBoolType | KTriType => No
      case KIntType | KHexType | KStringType => Literal("")
    }
    defs ::: List(Default(baseDef, Yes))
  }
  
  def mkAChoice(c: CChoice) = c match {
    case CChoice(_,Prompt(_,vis),isBool,isMand,defs,cs) =>
      AChoice(vis, isBool, isMand, cs map { _.name })
  }

  /**
   * Creates the abstract syntax representation of a concrete KConfig model.
   *
   * @author Steven She (shshe@gsd.uwaterloo.ca)
   */
  class AbstractSyntaxBuilder(k: ConcreteKConfig) {

    import TypeFilterList._


    /**
     * A map from an identifier to its select expressions
     */
    lazy val revMap : Map[String, Set[KExpr]] = {
      val mutMap = new HashMap[String, collection.mutable.Set[KExpr]]
              with MultiMap[String, KExpr]
      Rewriter.everywheretd {
        Rewriter.query {
          case CConfig(_,name,_,_,_,_,_,sels,_,_,_) =>
            sels.map { case Select(n,e) => (n, Id(name) && e) }.foreach {
              case (k,v) => mutMap addBinding (k,v)
            }
        }
      }(k)

      (Map() withDefaultValue (Set(): Set[KExpr])) ++
              (mutMap.iterator map { case (k,v) => (k, Set() ++ v) })
    }

    /**
     * Creates the reverse dependency expression for a config
     */
    def rev(n: String) = revMap(n).toList

    /**
     * Defaults operate such that the first default that is 'active'(e.g.
     * has a value of 'm' or 'y') takes effect.
     */
    def toADefaults(defs: List[Default]): List[ADefault] = {

      def t(prev: List[KExpr], next: List[Default]): List[ADefault] = next match {
        case Nil => Nil
        case Default(value, cond) :: tail =>
          ADefault(value, prev, cond) :: t(cond::prev, tail)
      }

      t(Nil, defs)
    }


    lazy val toAbstractSyntax : AbstractKConfig = {

      val configs = Rewriter.collectl {
        case CConfig(_,name,_,t,inh,ps,defs,_,rngs,_,_) =>
          val pro = ((No: KExpr) /: ps){ _ || _.cond }
          AConfig(name, t, inh, pro, toADefaults(addBaseDefault(t, defs)), rev(name), rngs)
      }(k)

      val choices = Rewriter.collectl {
        case c: CChoice => mkAChoice(c)
      }(k)

      AbstractKConfig(configs, choices)
    }
  }
}