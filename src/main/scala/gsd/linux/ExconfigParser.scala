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

import scala.util.parsing.combinator._
import util.parsing.input.{PagedSeqReader, Reader}
import collection.immutable.PagedSeq

import TypeFilterList._
import java.io.{InputStreamReader, InputStream}
import org.kiama.rewriting.Rewriter

/**
 * A parser for the Kconfig extract file (.exconfig).
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
class ExconfigParser extends KExprParser with ImplicitConversions {

  //Adds support to @{link stringLiteral} for escaping quotes
  private lazy val strLiteral =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt"])*"""+"\"").r ^^
      {
        s => s.substring(1, s.length - 1).replaceAll("\\\\\"","\"")
      }

  private lazy val kType: PackratParser[KType] =
    "boolean"  ^^^ KBoolType |
    "tristate" ^^^ KTriType |
    "integer"  ^^^ KIntType |
    "hex"      ^^^ KHexType |
    "string"   ^^^ KStringType

  private lazy val exExpr: PackratParser[KExpr] =
    "[" ~> opt(expr) <~ "]" ^^ { _.getOrElse(Yes) }

  private lazy val ifExpr: PackratParser[KExpr] =
    "if" ~> exExpr

  private lazy val idString: PackratParser[String] =
    identifier ^^ { case Id(s) => s }

  private lazy val env =
    "env" ~> idString ~ ifExpr ^^ Env

  private lazy val prompt =
    "prompt" ~> strLiteral ~ ifExpr ^^ Prompt

  private lazy val select =
    "select" ~> idString ~ ifExpr ^^ Select

  private lazy val default =
    "default" ~> exExpr ~ ifExpr ^^ Default

  private lazy val range =
    "range" ~> "[" ~> (idOrValue) ~ (idOrValue <~ "]") ~ ifExpr ^^ Range

  private lazy val inherited =
    "inherited" ~> exExpr

  private lazy val depends: PackratParser[DependsOn] =
    "depends" ~> "on" ~> exExpr ^^ DependsOn

  private lazy val properties =
    rep(prompt | depends | default | range | select | env)

  private lazy val kconfig: PackratParser[ConcreteKConfig] =
    syms ^^ { c => ConcreteKConfig(addNodeIds(CMenu(-1, Prompt(KConfigParser.rootId,Yes), c))) }

  private lazy val syms: PackratParser[List[CSymbol]] =
    rep(menu | config | choice | ifSym | comment)

  private lazy val ifSym: PackratParser[CIf] =
  "if" ~> exExpr ~ ("{" ~> syms <~ "}") ^^
    {
      case expr~cs =>
        CIf(-1, expr, cs)
    }

  private lazy val menu =
    "menu" ~> strLiteral ~ ("{" ~> (opt(depends) ^^ { _.getOrElse(DependsOn(Yes)) })) ~ syms <~ "}" ^^
      {
        case name~DependsOn(cond)~children =>
          CMenu(-1, Prompt(name, cond), children)
      }

  private lazy val comment =
    "comment" ~> strLiteral ~ ("{" ~> (opt(depends) ^^ (_.getOrElse(DependsOn(Yes)))) <~ "}") ^^
      {
        case text~DependsOn(cond) => CComment(-1, text, cond)
      }

  private lazy val choice =
    "choice" ~> (kType ^^ { _ == KBoolType }) ~
      (opt("optional") ^^ { !_.isDefined }) ~ ("{" ~> rep(prompt|default|depends)) ~
      rep(config) <~ "}" ^^
        {
          case isBool~isMand~props~cs =>
            val properties = mkProperties(props)
            CChoice(-1, properties.prompts.head, isBool, isMand, properties.defaults, cs)
        }

  private def mkProperties(props: List[Any]): Properties =
    Properties(prompts = props collect { case x: Prompt => x },
      defaults = props collect { case x: Default => x },
      selects = props collect { case x: Select => x },
      ranges = props collect { case x: Range => x },
      dependsOn = props collect { case x: DependsOn => x })


  private case class Properties(prompts: List[Prompt] = Nil,
                                defaults: List[Default] = Nil,
                                selects: List[Select] = Nil,
                                ranges: List[Range] = Nil,
                                dependsOn: List[DependsOn] = Nil)


  private lazy val config =
    ("config" ^^^ false | "menuconfig" ^^^ true) ~ identifier ~
            kType ~ ("{" ~> properties) ~ opt(inherited) ~ syms <~ "}" ^^
      {
        case isMenuConfig~Id(name)~t~props~inh~children =>

          val (p, inheritedExpr) =
              (mkProperties(props), inh getOrElse Yes)

          CConfig(-1, name, isMenuConfig, t,
                  inheritedExpr,
                  p.prompts,
                  p.defaults,
                  p.selects,
                  p.ranges,
                  p.dependsOn,
                  children)
      }

  def addNodeIds(root: CMenu): CMenu = {
    import Rewriter._
    var i = 0
    def nextId = {
      i += 1
      i
    }
    rewrite {
      everywheretd {
        rule {
          case s: CMenu => s.copy(nId = nextId)
          case s: CConfig => s.copy(nId = nextId)
          case s: CChoice => s.copy(nId = nextId)
          case s: CComment => s.copy(nId = nextId)
        }
      }
    }(root)
  }

  def parseKConfigStream(stream: InputStream): ConcreteKConfig =
    succ(parseAll(kconfig, new PagedSeqReader(PagedSeq fromReader new InputStreamReader(stream))))

  def parseKConfig(str: String): ConcreteKConfig =
    succ(parseAll(kconfig, str))

  def parseKConfigFile(file: String): ConcreteKConfig =
    succ(parseAll(kconfig, new PagedSeqReader(PagedSeq fromFile file)))

}

