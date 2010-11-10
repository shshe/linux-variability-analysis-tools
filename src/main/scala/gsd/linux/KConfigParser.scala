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

/**
 * A parser for the Kconfig extract file (.exconfig).
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object KConfigParser extends KExprParser with ImplicitConversions {

  val rootId = "Linux Kernel Configuration"

  //Adds support to @{link stringLiteral} for escaping quotes
  private lazy val strLiteral =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt"])*"""+"\"").r ^^
      {
        s => s.substring(1, s.length - 1)
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

  private lazy val property =
    prompt | depends | default | range | select | env

  private lazy val kconfig: PackratParser[ConcreteKConfig] =
  syms ^^ { c => ConcreteKConfig(CMenu(Prompt(rootId,Yes), c)) }

  //FIXME temporary hack to get around ignoring if statements
  private lazy val syms: PackratParser[List[CSymbol]] =
    rep {
      menu ^^ { List(_) } |
      config ^^ { List(_) } |
      choice ^^ { List(_) } |
      ifSym
    } ^^ { _.flatten[CSymbol] }


  //FIXME ignoring if statements
  private lazy val ifSym =
  "if" ~> exExpr ~ ("{" ~> syms <~ "}") ^^
    {
       case e~children => children
    }

  private lazy val menu =
    "menu" ~> {
      strLiteral ~ {
        "{" ~> opt(depends) ^^ { d => ( d getOrElse DependsOn(Yes) ).cond }
      } ^^ Prompt
    } ~ syms <~ "}" ^^ CMenu

  private lazy val choice =
    "choice" ~> (kType ^^ { _ == KBoolType }) ~
      (opt("optional") ^^ { !_.isDefined }) ~ ("{" ~> rep(prompt|default|depends)) ~
      rep(config) <~ "}" ^^
        {
          case isBool~isMand~props~cs =>
            val p = props.typeFilter[Prompt].head
            CChoice(p,isBool,isMand,props.typeFilter[Default],cs)
        }

  private lazy val config =
    ("config" ^^^ false | "menuconfig" ^^^ true) ~ identifier ~
            kType ~ opt("{" ~> rep(property) ~ opt(inherited) ~ syms <~ "}") ^^
      {

        case isMenuConfig~Id(n)~t~Some(props~inh~syms) =>
          mkConfig(n, isMenuConfig, t, props.typeFilter[Property],
                   inh.getOrElse(Yes), props.typeFilter[DependsOn], syms)

        case isMenuConfig~Id(n)~t~None =>
          mkConfig(n, isMenuConfig, t, Nil, Yes, Nil, Nil)
        
      }

  def parseKConfig(stream: Reader[Char]): ConcreteKConfig =
    succ(parseAll(kconfig, stream))

  def parseKConfig(str: String): ConcreteKConfig =
    succ(parseAll(kconfig, str))

  def parseKConfigFile(file: String): ConcreteKConfig =
    succ(parseAll(kconfig, new PagedSeqReader(PagedSeq fromFile file)))

  /**
   * Creates a config with a placeholder for visibility condition
   */
  def mkConfig(id: String, isMenuConfig: Boolean, t: KType, props: List[Property],
               inherited: KExpr, depends: List[DependsOn], cs: List[CSymbol]) = {
    val prompt = props.typeFilter[Prompt]
    val defs = props.typeFilter[Default]
    val sels = props.typeFilter[Select]
    val ranges = props.typeFilter[Range]
    CConfig(id, isMenuConfig, t, inherited, prompt, defs, sels, ranges, depends, cs)
  }

}

