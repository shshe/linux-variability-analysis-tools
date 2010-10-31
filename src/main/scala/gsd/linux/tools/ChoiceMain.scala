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

import java.io.PrintStream

/**
 * Outputs all choice members as a comma-separated list of feature ids.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object ChoiceMain {

  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println("Parameters: <extract file> [output file]")
      System.exit(1)
    }

    val extract = KConfigParser.parseKConfigFile(args first)
    val out = if (args.size > 1) new PrintStream(args(1)) else System.out

    val mutex = extract.choices filter {
      case CChoice(_,true,false,_,_) => true
      case _ => false
    }
    val xor   = extract.choices filter {
      case CChoice(Prompt(_,Yes),true,true,_,_) => true
      case _ => false
    }
    val xorC  = (extract.choices filter {
      case CChoice(_,true,true,_,_) => true
      case _ => false
    }) -- xor
    val or    = extract.choices filter {
      case CChoice(_,false,true,_,_) => true
      case _ => false
    }

    println("=== Mutex Groups ===")
    mutex foreach { g =>
      out println (g.children map { _.id } mkString ",")
    }

    println("=== Xor Groups ===")
    xor foreach { g =>
      out println (g.children map { _.id } mkString ",")
    }

    println("=== Conditional Xor Groups ===")
    xorC foreach { g =>
      out println (g.children map { _.id } mkString ",")
    }

    println("=== Or Groups ===")
    or foreach { g =>
      out println (g.children map { _.id } mkString ",")
    }
  }
}