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

package gsd.linux.stats

import gsd.linux.{AbstractSyntaxWriter, AbstractSyntax, KConfigParser}
import java.io.PrintStream

/**
 * Outputs the abstract Kconfig model to a specified output stream.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object AbstractSyntaxMain extends AbstractSyntax with AbstractSyntaxWriter {

  def main(args: Array[String]) : Unit = {
    val out = if (args.size > 1) new PrintStream(args(1)) else System.out
    
    KConfigParser.parseKConfigFile(args(0))
                 .toAbstractSyntax
                 .printGrammar(out)

    out.close
  }

}
