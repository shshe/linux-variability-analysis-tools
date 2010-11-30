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

import java.io.PrintStream

import gsd.linux._
import KConfigParser._
import Hierarchy._

/**
 * Outputs the config hierarchy of a Kconfig model. The config hierarchy is one
 * that only consists of configs and menuconfigs. Menus and Choices are removed.
 * Choice members are considered configs.
 *
 * The special symbol "^" is used to denote the synthetic root.
 *
 * @author Steven She (shshe@gsd.uwaterloo.ca)
 */
object HierarchyMain {

  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      System.err.println(
        "HierarchyMain <kconfig-extract-file> [<out-file>]")
      System exit 1
    }

    val k = parseKConfigFile(args head)
    val out = if (args.size > 1) new PrintStream(args(1))
                 else System.out

    toStringMap(mkConfigMap(k), k.allConfigs, "^").foreach { case (k,v) =>
      out.println(k + "," + v)
    }
    out.close
  }
}