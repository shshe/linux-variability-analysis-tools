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

package gsd.linux.tools

import gsd.linux._

import java.io.PrintStream

/**
 * Outputs the boolean translation of a Kconfig extract. 
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object TristateTranslationMain {

  def main(args: Array[String]) : Unit = {
    if (args.size == 0) {
      System.err.println(
        "TristateTranslation Main <Kconfig-extract-file> [<output-file>]")
      System exit 1
    }

    val out = if (args.size > 1) new PrintStream(args(1))
                 else System.out

    val k = KConfigParser.parseKConfigFile(args head)

    //First output identifiers
    for (id <- k.identifiers) out.println("@ " + id)

    val trans = new TristateTranslation(k)
    val exprs = trans.translate

    for (id <- trans.generated) out.println("$ " + id)
    for (e  <- exprs) out.println(e)

    out.close
  }
}