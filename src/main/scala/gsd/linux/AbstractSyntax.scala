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

import org.kiama.rewriting.Rewriter._
import collection.mutable.{MultiMap, HashMap}

/**
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait AbstractSyntax {
  implicit def toAbstractSyntax(k: ConcreteKConfig) =
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
    case CChoice(Prompt(_,vis),isBool,isMand,defs,cs) =>
      AChoice(vis, isBool, isMand, cs map { _.id })
  }


}

object AbstractSyntax extends AbstractSyntax

/**
 * Creates the abstract syntax representation of a concrete KConfig model.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
class AbstractSyntaxBuilder(k: ConcreteKConfig) extends AbstractSyntax
        with TypeFilterList {

  /**
   * Helper function for finding a particular config, probably belongs elsewhere
   */
  def find(id: String): Option[CConfig] = {
    var result : Option[CConfig] = None
    oncetd {
      rule {
        case c: CConfig if c.id == id => result = Some(c)
      }
    }(k)
    result
  }

  /**
   * A map from an identifier to its select expressions
   */
  lazy val revMap : Map[String, Set[KExpr]] = {
    val mutMap = new HashMap[String, collection.mutable.Set[KExpr]]
                        with MultiMap[String, KExpr]
    everywheretd {
      query {
        case CConfig(id,_,_,_,_,_,sels,_,_,_) =>
          sels.map { case Select(n,e) => (n, Id(id) && e) }.foreach {
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

  lazy val toAbstractSyntax : AbstractKConfig = {

    val configs = collectl {
      case CConfig(id,_,t,inh,p,defs,_,rngs,_,_) =>
        val pro = p match {
          case Some(Prompt(_,cond)) => cond
          case None => No
        }
        AConfig(id,t,inh,pro,addBaseDefault(t, defs),rev(id),rngs)
    }(k)
    
    val choices = collectl {
      case c: CChoice => mkAChoice(c)
    }(k)
    
    AbstractKConfig(configs, choices)
  }
}