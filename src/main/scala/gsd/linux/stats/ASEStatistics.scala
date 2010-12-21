package gsd.linux.stats
import gsd.linux._

class ASEStatistics(ck: ConcreteKConfig) extends FeatureStatistics(ck) {

  // Feature Kinds
  // menus, menuconfigs, configs, choice

  // Switch features
  lazy val boolType = boolConfigs
  lazy val tristateType = triConfigs

  // Post-processed ConcreteKConfig
  lazy val ppk = ASEStatistics.removeInheritedAndDependsOn(ck)

  lazy val apk = ppk.toAbstractKConfig

  // Explicitly defined prompt (i.e. condition not propagated by an inherited
  // or depends on condition)
  lazy val configsWithVisConds = ppk.allConfigs filter { c =>
    c.prompt exists { _.cond != Yes }
  }

  lazy val configsWithDefaults = ppk.allConfigs filter { c =>
    !c.defs.isEmpty
  }

  // Configs with defaults that have the same condition as its prompt
  lazy val configsWithTrueDefs = configsWithDefaults filter { c =>
    c.defs exists { d => c.prompt exists { _.cond == d.cond } }
  } distinct

  // Sub-category: defaults with expressions that are not simple values
  // default 'y' acts like a computed default
  lazy val configsWithTrueCompDefs = configsWithTrueDefs filter { c =>
    c.defs exists { d => !d.iv.isInstanceOf[Value] || d.iv == Yes && d.cond != Yes }
  }

  // Sub-category: defaults with expressions that are simple values
  lazy val configsWithTrueLitDefs = configsWithTrueDefs filter { c =>
    c.defs exists { d => d.iv.isInstanceOf[Value] && (d.iv != Yes || d.cond == Yes) }
  }

  // Configs with defaults that have a condition different from its prompt
  lazy val configsWithCondDerived = configsWithDefaults filter { c =>
    c.defs exists  { d => c.prompt exists { _.cond != d.cond } }
  } distinct

  // Configs that are NEVER user-selectable
  lazy val configsWithUncondDerived = ppk.allConfigs filter { c =>
    c.prompt forall { _.cond == No }
  }

  lazy val configsWithUncondDerivedAndDefs = configsWithUncondDerived filter { c =>
    !c.defs.isEmpty
  }

  lazy val configsWithUncondDerivedAndNoDefs = configsWithUncondDerived filter { c =>
    c.defs.isEmpty
  }

  lazy val configsWithUncondDerivedAndRevDeps  =
    configsWithUncondDerived filter { c1 =>
      !apk.configs.find { c2 => c2.id == c1.id }.get.rev.isEmpty
    }

  lazy val configsWithRevDeps =
    apk.configs filter { !_.rev.isEmpty }

}

object ASEStatistics {

  import KExprList._
  import org.kiama.rewriting.Rewriter._

  /**
   * Removes a conjunction from the condition of a property
   */
  def removeCondition[A <: Property](p: A, conj: KExpr): KExpr = {
    val cj = conj.splitConjunctions
    p.cond.splitConjunctions filterNot
      { cj contains } filter
      { _ != Yes } mkConjunction
  }

  def rewriteProperties(ck: ConcreteKConfig)(f: (CConfig, Property) => KExpr)
      : ConcreteKConfig = {
    val strategy =
      everywheretd {
        rule {
          case config@CConfig(_,_,_,inh,pros,ds,selects,rngs,_,_) =>
            config.copy (
              prompt = pros map
                { p => p.copy( c = f(config, p) ) },

              defs = ds map
                { d => d.copy( c = f(config, d) ) },

              sels = selects map
                { s => s.copy( c = f(config, s) ) },

              ranges = rngs map
                { r => r.copy( c = f(config, r) ) }
            )
        }
      }
    rewrite(strategy)(ck)
  }

  /**
   * Removes inherited expression from property conditions.
   */
  def removeInherited(ck: ConcreteKConfig): ConcreteKConfig =
    rewriteProperties(ck){ (config, p) =>
      removeCondition(p, config.inherited)
    }

  /**
   * Removes depends on expression from property conditions.
   */
  def removeDependsOn(ck: ConcreteKConfig): ConcreteKConfig =
    rewriteProperties(ck){ (config, p) =>
      removeCondition(p, config.depends map { _.cond } mkConjunction)
    }

  def removeInheritedAndDependsOn(ck: ConcreteKConfig): ConcreteKConfig =
    rewriteProperties(ck){ (config, p) =>
      removeCondition(p, (config.depends map { _.cond } mkConjunction) && config.inherited)
    }


}

object ASEStatisticsMain {

  def representationStats(implicit stats: ASEStatistics) {
    println("Bool:   " + stats.boolType.size)
    println("Tri:    " + stats.tristateType.size)

    println
    println("Int:    " + stats.intConfigs.size)
    println("Hex:    " + stats.hexConfigs.size)
    println("String: " + stats.stringConfigs.size)

    println
    println("Menus:  " + stats.menus.size)
  }

  def groupingStats(implicit stats: ASEStatistics) {
    println("Menus:        " + stats.menus.size)
    println("Menuconfigs:  " + stats.menuconfigs.size)

    println
    println("XOR:          " + stats.xorGroups.size)
    println("OR:           " + stats.orGroups.size)
    println("MUTEX:        " + stats.mutexGroups.size)
  }

  def dependencyStats(implicit stats: ASEStatistics) {
    // visibility conditions
    println("Visibility Conditions: %4d / %4d".format(stats.configsWithVisConds.size, stats.allConfigs.size))

    println

    // TODO categories can overlap
    println("With Defaults:         %4d / %4d".format(stats.configsWithDefaults.size, stats.allConfigs.size))

    // explicit defaults (soft constraints)
    println("  True Defs:           %4d / %4d".format(stats.configsWithTrueDefs.size, stats.configsWithDefaults.size))

    //    computed (expressions)
    println("    Computed:          %4d / %4d".format(stats.configsWithTrueCompDefs.size, stats.configsWithTrueDefs.size))

    //    literals
    println("    Literals:          %4d / %4d".format(stats.configsWithTrueLitDefs.size, stats.configsWithTrueDefs.size))

    //    conditionally derived
    println("  Cond. Derived:       %4d / %4d".format(stats.configsWithCondDerived.size, stats.configsWithDefaults.size))


    // unconditionally derived
    println("Uncond. Derived:       %4d / %4d".format(stats.configsWithUncondDerived.size, stats.configsWithDefaults.size))
    println("  With Default:        %4d / %4d".format(stats.configsWithUncondDerivedAndDefs.size, stats.configsWithUncondDerived.size))
    println("  With No Default:     %4d / %4d".format(stats.configsWithUncondDerivedAndNoDefs.size, stats.configsWithUncondDerived.size))
    println("  With Rev Deps:       %4d / %4d".format(stats.configsWithUncondDerivedAndRevDeps.size, stats.configsWithUncondDerived.size))

    println
    // Reverse dependencies
    println("Reverse Dependencies:  %4d / %4d".format(stats.configsWithRevDeps.size, stats.allConfigs.size))

  }

  def intersectionStats(implicit stats: ASEStatistics) {
    import Intersection._

    val res =
      calculatePairWise(Array((stats.configsWithUncondDerived map { _.id }).toSet,
                      (stats.configsWithDefaults map { _.id }).toSet,
                      (stats.configsWithRevDeps map { _.id }).toSet))

    printPairWiseSizes(res, Array("Uncond. Derived", "w/Defaults", "w/Rev Deps"))

    println

    val combos =
      calculatePartitions(
        Array((stats.configsWithUncondDerived map { _.id }).toSet,
              (stats.configsWithCondDerived map { _.id }).toSet,
              (stats.configsWithTrueDefs map { _.id }).toSet),
        Array((stats.configsWithDefaults map { _.id }).toSet,
              (stats.configsWithRevDeps map { _.id }).toSet),
        Array("w/Defaults", "w/RevDep"))

    printComboSizes(combos, Array("Uncond.Derived", "Cond.Derived", "True Def."))
  }

  def main(args: Array[String]) {
    println("Parsing exconfig...")
    val k = KConfigParser.parseKConfigFile("/home/shshe/Dev/extracts/2.6.32.exconfig")
    implicit val stats = new ASEStatistics(k)

//    println
//    representationStats
//    println
//    groupingStats
    println
    dependencyStats
    println
    intersectionStats
  }

}