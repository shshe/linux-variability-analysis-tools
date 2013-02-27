/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2012 Steven She <shshe@gsd.uwaterloo.ca>
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

// package gsd.linux.analysis
//
// import gsd.linux._
// import annotation.tailrec
//
//
// object TypeCheck {
//
//
//   /**
//    * @return Some(x) if a type error occurs where x is the invalid sub-expression
//    *         None if the expression is valid
//    */
//   def check(id2ktype: Map[String, KType])(expr: KExpr, expectedType: KType): Option[KExpr] = {
//
//     def _check(e: KExpr): Either[KType, KExpr] ={
//
//       def _binResult(l: Either[KType, KExpr], r: Either[KType, KExpr]): Either[KType, KExpr] = (l, r) match {
//         case (Left(lt), Left(rt)) if lt == rt => l
//         case (Left(lt), Left(rt)) => Right(e)
//         case (Right(errorExpr), _) => Right(errorExpr)
//         case (_, Right(errorExpr)) => Right(errorExpr)
//       }
//
//       expr match {
//         case And(l,r) => _binResult(_check(l), _check(r))
//         case Or(l,r)  => _binResult(_check(l), _check(r))
//         case Eq(l,r)  => _binResult(_check(l), _check(r))
//         case NEq(l,r) => _binResult(_check(l), _check(r))
//         case NonCanonEq(l,r) => _binResult(_check(l), _check(r))
//         case Not(l)   => _check(l)
//         case KInt(_) => Left(KIntType)
//         case KHex(_) => Left(KHexType)
//         case Literal(_) => Left(KStringType)
//         case Yes | Mod | No => Left(KTriType)
//         case Id(name) => Left(id2ktype(name))
//       }
//     }
//     _check(expr) match {
//       case Left(_) => None
//       case Right(invalidExpr) => Some(invalidExpr)
//     }
//   }
//
//   // TODO this check
//   def check(id2ktype: Map[String, KType])(config: CConfig): List[KExpr] = config match {
//     case CConfig(_,_,_,ktype,_,prompts,defaults,selects,ranges,_,_,_) =>
//       //TODO
//   }
//
//
//   def main(args: Array[String]) {
//     val k = KConfigParser.parseKConfigFile(args(0))
//     val id2ktype = k.configMap.map {
//       case (id, config) => id -> config.ktype
//     }.toMap
//
//     // FIXME check all expressions
//   }
//
// }
