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

import util.parsing.input.Reader
import util.parsing.combinator.{PackratParsers, ImplicitConversions, JavaTokenParsers}

/**
 * A parser for reading Kconfig expressions. It is currently only used by
 * the KConfigParser.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
trait KExprParser extends JavaTokenParsers with PackratParsers with ImplicitConversions {

  val identifier = """[-_0-9a-zA-Z]+""".r ^^ Id

  val lit = ("\"[^\"]*\"").r ^^
      {
        case "\"y\"" => Yes
        case "\"Y\"" => Yes
        case "\"m\"" | "\"M\"" => Mod
        case "\"n\"" | "\"N\"" => No
        case str => Literal(str.substring(1, str.length - 1))
      }

  val hex = """0x[0-9a-fA-F]+""".r ^^ KHex

  val idOrValue : PackratParser[IdOrValue] =
    "<choice>" ^^^ Yes | lit | hex | "[-_0-9a-zA-Z]+".r ^^
      {
        case "y" | "Y" => Yes
        case "m" | "M" => Mod
        case "n" | "N" => No
        case s =>
          try {
            KInt(s.toInt)
          }
          catch {
            case _ : NumberFormatException => Id(s)
          }
      }

  lazy val expr : PackratParser[KExpr] =
    (andExpr ~ ("||" ~> expr)) ^^ Or | andExpr

  lazy val andExpr : PackratParser[KExpr] =
    (eqExpr ~ ("&&" ~> andExpr)) ^^ And | eqExpr

  lazy val eqExpr : PackratParser[KExpr] =
    (idOrValue ~ ("="|"!=") ~ idOrValue) ^^
       {
         case l~"="~r  => Eq(l,r)
         case l~"!="~r => NEq(l,r)
       } |
    unaryExpr

  lazy val unaryExpr : PackratParser[KExpr] =
    "!" ~> primaryExpr ^^ Not | primaryExpr

  lazy val primaryExpr = "(" ~> expr <~ ")" | idOrValue

  def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => sys.error(x.toString)
  }

  // FIXME different behaviour from overloaded method
  def parseKExpr(stream : Reader[Char]): KExpr = succ(parseAll(expr, stream))

  def parseKExpr(str : String): KExpr =
    if (str.length == 0) Yes
    else succ(parseAll(expr, str))

  def parseId(str: String) = succ(parseAll(identifier, str))

  def parseIdOrValue(str: String) = succ(parseAll(idOrValue, str))
}

object KExprParser extends KExprParser
