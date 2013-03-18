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
import Rewriter._
import annotation.tailrec

/**
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object AbstractSyntax {

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
    case CChoice(_,Prompt(_,vis),isBool,isMand,defs,_,cs) =>
      AChoice(vis, isBool, isMand, cs map { _.name })
  }



  /** Strategies to rewrite 'm' to MODULES **/

  // Fail if we detect an equality / inequality
  val sModEq = strategy {
    case Eq(Mod, _) | Eq(_,Mod) | NEq(Mod, _) | NEq(_,Mod) => None
    case x => Some(x)
  }

  val sMod = rule {
    case Mod => Id("MODULES")
  }

  // if ModEq succeeds (i.e. not equality / inequality with Mod), then
  // apply try to apply sMod. If sMod fails, continue with s (i.e. recurse)
  //
  val sDetectMod: Strategy = some(sModEq < (sMod <+ sDetectMod) + fail)
  
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
          case CConfig(_,name,_,_,_,_,_,sels,_,_,_,_) =>
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

      val parentMap = new collection.mutable.HashMap[AConfig, AConfig]()
      val envConfigs = new collection.mutable.ListBuffer[String]()

      // Traverse tree and make changes
      def dfs(parent: Option[AConfig])(curr: CSymbol): List[AConfig] = curr match {
        case CConfig(id,name,_,t,inh,ps,defs,_,rngs,envs,dependsOns,children) =>
          val pro = ((No: KExpr) /: ps){ _ || _.cond }

          // Detect if '&& m' exists in the depends on clause
          val modOnly = sDetectMod(dependsOns).isDefined

          val ac = AConfig(id,name, t, inh, pro, toADefaults(addBaseDefault(t, defs)), rev(name), rngs, modOnly)

          // parent map
          if (parent.isDefined)
            parentMap += parent.get -> ac

          if (!envs.isEmpty)
            envConfigs += name

          ac :: (children flatMap dfs(Some(ac)))
          
        case x => 
          x.children flatMap dfs(parent)
      }

      // Push choice visibility down to the choice members
      val withChoiceVis =
        everywheretd {
          rule {
            case x@CChoice(_, Prompt(_, vis), _, _, _,_, children) =>
              x.copy (cs = children map {
                case config => config.copy(prompt = config.prompt map {
                  case Prompt(text, expr) => Prompt(text, expr && vis)
                })
              })
          }
        }

      val withChoiceDeps =
        everywheretd {
          rule {
            case x@CChoice(_, _, _, _, _, deps, children) =>
              x.copy (cs = children map {
                case config => config.copy(depends = config.depends ::: deps)
              })
          }
        }

      val configs = dfs(None)(rewrite(withChoiceVis + withChoiceDeps)(k.root))

      // Retain only one definition per config in the case of multiple definitions
      val distinct = new collection.mutable.ListBuffer[AConfig]
      @tailrec
      def distinctConfigs(rest: List[AConfig]) {
        rest match {
          case Nil => Nil
          case head::tail =>
            distinct += head
            distinctConfigs(tail dropWhile (_.name == head.name))
        }
      }
      
      distinctConfigs(configs sortBy (_.name))

      val choices = collectl {
        case c: CChoice => mkAChoice(c)
      }(k)
      
      AbstractKConfig(distinct.toList, choices, parentMap.toMap, envConfigs.toList)
    }
  }
}