package gsd.linux.stats

import gsd.linux._
import org.kiama.rewriting.Rewriter

trait VisibilityStatistics {
  this: ASEStatistics =>

  // Must have a prompt defined, but with a condition == 'yes'
  lazy val configsWithNoVisConds = ppk.allConfigs filter { c =>
    !c.prompt.isEmpty && (c.prompt forall { _.cond == Yes })
  }

  // Explicitly defined prompt (i.e. condition not propagated by an inherited
  // or depends on condition)
  lazy val configsWithVisConds = ppk.allConfigs filter { c =>
    c.prompt exists { _.cond != Yes }
  }

  //
  // The next four statistics should partition the set of configs
  //


  // Configs that are NEVER user-selectable
  lazy val configsWithUncondDerived = ppk.allConfigs filter { c =>
    c.prompt.isEmpty || (c.prompt forall { p => p.cond == No })
  }

  // Configs that are user-selectable, and no have no default
  lazy val configsWithNoDefaults = ppk.allConfigs filter { c=>
    !c.prompt.isEmpty && c.defs.isEmpty
  }

  // Configs with defaults that have the same condition as its prompt
  lazy val configsWithTrueDefs = (ppk.allConfigs filter { c =>
    !c.prompt.isEmpty && !c.defs.isEmpty && (c.defs forall { d => c.prompt exists { _.cond == d.cond } })
  })

  // Configs with defaults that have a condition different from its prompt
  lazy val configsWithCondDerived = (ppk.allConfigs filter { c =>
    !c.prompt.isEmpty && !c.defs.isEmpty && (c.defs exists { d => c.prompt exists { _.cond != d.cond } })
  })

  //
  // ------------------------------------------------------------------------
  //


  lazy val configsWithRevDeps =
    apk.configs filter { !_.rev.isEmpty }

  lazy val defaultsValued =
    defaults filter { _.iv.isInstanceOf[Value] }

  lazy val defaultsComputed =
    defaults filter { !_.iv.isInstanceOf[Value] }

}