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
 * Outputs the boolean translation of a Kconfig extract. 
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object BooleanTranslationMain {

  def main(args: Array[String]) : Unit = {
    if (args.size == 0)
      System.err.println("Parameters: 1. Extract file, 2. Output file (optional)")

    val out = if (args.size > 1) new PrintStream(args(1))
                 else System.out

    val k = KConfigParser.parseKConfigFile(args head)

    //First output identifiers
    for (id <- k.identifiers) out.println("@ " + id)

    val btrans = BooleanTranslation.mkBooleanTranslation(k.toAbstractKConfig)
    for (id <- btrans.genVars) out.println("$ " + id)
    for (e  <- btrans.exprs)   out.println(e)

    out.close
  }
}