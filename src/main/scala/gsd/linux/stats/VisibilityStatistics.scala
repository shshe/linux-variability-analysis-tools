package gsd.linux.stats

import gsd.linux._
import org.kiama.rewriting.Rewriter

trait VisibilityStatistics {
  this: ASEStatistics =>



  // Must have a prompt defined, but with a condition == 'yes'
  lazy val configsWithNoVisConds = ppk.allConfigs filter { c =>
    (c.prompt.size == 1) && (c.prompt forall { _.cond == Yes })
  }

  // Conditionally derived
  lazy val configsWithVisConds = ppk.allConfigs filter { c =>
    (c.prompt.size == 1) && (c.prompt exists { _.cond != Yes })
  }

  // Conditionall derived
  lazy val configsWithMultiplePrompts = ppk.allConfigs filter { c =>
    c.prompt.size > 1
  }

  // Configs that are NEVER user-selectable
  lazy val configsWithUncondDerived = ppk.allConfigs filter { c =>
    c.prompt.isEmpty
  }



  //
  // Derivation statistics
  //
  lazy val derivedConfigs = configsWithUncondDerived

  lazy val condDerivedConfigs =
    (configsWithVisConds ++ configsWithMultiplePrompts ++
      (configsWithRevDeps filterNot { configsWithUncondDerived.contains })).toSet

  lazy val userDefinedConfigs =
    configsWithNoVisConds filterNot { configsWithRevDeps.contains }



  // Configs with defaults that have the same condition as its prompt
  lazy val configsWithTrueDefs = ppk.allConfigs filter { c =>
    c.defs exists { d => c.prompt exists { _.cond == d.cond } }
  } distinct

  // Configs with defaults that have a condition different from its prompt
  lazy val configsWithCondDerived = ppk.allConfigs filter { c =>
    c.defs exists  { d => c.prompt exists { _.cond != d.cond } }
  } distinct


  lazy val configsWithRevDeps =
    apk.configs filter { !_.rev.isEmpty }

  lazy val defaultsValued =
    defaults filter { _.iv.isInstanceOf[Value] }

  lazy val defaultsComputed =
    defaults filter { !_.iv.isInstanceOf[Value] }

}