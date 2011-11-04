/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2011 Steven She <shshe@gsd.uwaterloo.ca>
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

import gsd.linux._

class TSEStatistics(val ck: ConcreteKConfig) {

  val ppk = ASEStatistics.fixHexIdentifiers(ASEStatistics.removeInheritedAndDependsOn(ck))

  /**
   * S11 Number of features with an explicit visibility condition
   */
  val configsWithExplicitVisibilityConditions: List[CConfig] =
    ppk.allConfigs filter { c =>
      c.prompt exists { p => p.cond != Yes }
    }

  /** NOTE: Menus don't store the inherited expressions */
  val menusWithExplicitVisibilityConditions: List[CMenu] =
    ppk.menus filter { m =>
      m.prompt.cond != Yes
    }

  /** NOTE: Choices don't store the inherited expressions */
  val choicesWithExplicitVisibilityConditions: List[CChoice] =
    ppk.choices filter { c =>
      c.prompt.cond != Yes
    }


  // The following three statistics should PARTITION the configs into
  // three categories.
  val alwaysVisibleConfigs: List[CConfig] =
    ppk.allConfigs filter { c =>
      !c.prompt.isEmpty && (c.prompt forall { p => p.cond == Yes })
    }

  // S14 Number of conditionally / unconditionally derived features
  //
  // This is the same as features with an explicit visibility condition
  val conditionallyDerivedConfigs: List[CConfig] =
    ppk.allConfigs filter { c =>
      !c.prompt.isEmpty && (c.prompt exists { p => p.cond != Yes })
    }

  val derivedConfigs: List[CConfig] =
    ppk.allConfigs filter { c =>
      c.prompt.isEmpty || (c.prompt forall { p => p.cond == No })
    }

  // end partition

  val derivedConfigsUsingLiterals: List[CConfig] =
    derivedConfigs filter { c =>
      c.defs forall { case Default(iv,_) => iv.isInstanceOf[Value] }
    }

  val derivedConfigsUsingExpressions: List[CConfig] =
    derivedConfigs filterNot (derivedConfigsUsingLiterals contains)

  // S12 Number of features with explicit defaults
  val explicitDefaultConfigs: List[CConfig] =
    alwaysVisibleConfigs filter { c =>
      c.defs exists { _.cond != No }
    }

  // S13a
  val explicitDefaultConfigsUsingLiterals: List[CConfig] =
    explicitDefaultConfigs filter { c =>
      c.defs forall { case Default(iv,_) => iv.isInstanceOf[Value] }
    }

  // S13b
  val explicitDefaultConfigsUsingExpressions: List[CConfig] =
    explicitDefaultConfigs filterNot (explicitDefaultConfigsUsingLiterals contains)


}


object TSEStatistics {

  def main(args: Array[String]) {

    // Test with just Linux
    println("Parsing Kconfig...")
    val linux = KConfigParser.parseKConfigFile(args(0))
    val stats = new TSEStatistics(linux)

    println("Total number of configs: " + linux.allConfigs.size)
    println("Total number of choices: " + linux.choices.size)
    println("Total number of menus  : " + linux.menus.size)

    println("Configs with Explicit Visibility Conditions: " + stats.configsWithExplicitVisibilityConditions.size + " / " + stats.ck.allConfigs.size)
    println("Menus with Explicit Visibility Conditions (NOTE, not post-processed): " + stats.menusWithExplicitVisibilityConditions.size + " / " + stats.ck.menus.size)
    println("Choice with Explicit Visibility Conditions (NOTE, not post-processed): " + stats.choicesWithExplicitVisibilityConditions.size + " / " + stats.ck.choices.size)

    println

    println("Always visible configs: " + stats.alwaysVisibleConfigs.size)
    println("Conditionally derived configs: " + stats.conditionallyDerivedConfigs.size)
    println("Derived configs: " + stats.derivedConfigs.size)
    println("   using literals: " + stats.derivedConfigsUsingLiterals.size)
    println("   using expressions: " + stats.derivedConfigsUsingExpressions.size)

    println

    println("Explicit default configs: " + stats.explicitDefaultConfigs.size)
    println("   using literals: " + stats.explicitDefaultConfigsUsingLiterals.size)
    println("   using expressions: " + stats.explicitDefaultConfigsUsingExpressions.size)

  }


}

