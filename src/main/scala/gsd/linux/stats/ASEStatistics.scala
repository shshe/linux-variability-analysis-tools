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


  // Must have a prompt defined, but with a condition == 'yes'
  lazy val configsWithNoVisConds = ppk.allConfigs filter { c =>
    !c.prompt.isEmpty && (c.prompt forall { _.cond == Yes })
  }

  // Explicitly defined prompt (i.e. condition not propagated by an inherited
  // or depends on condition)
  lazy val configsWithVisConds = ppk.allConfigs filter { c =>
    c.prompt exists { _.cond != Yes }
  }

  // Configs with defaults that have the same condition as its prompt
  lazy val configsWithTrueDefs = ppk.allConfigs filter { c =>
    c.defs exists { d => c.prompt exists { _.cond == d.cond } }
  } distinct

  // Sub-category: defaults with expressions that are not simple values
  // default 'y' acts like a computed default
  lazy val configsWithTrueCompDefs = ppk.allConfigs filter { c =>
    c.defs exists { d => !d.iv.isInstanceOf[Value] || d.iv == Yes && d.cond != Yes }
  }

  // Sub-category: defaults with expressions that are simple values
  lazy val configsWithTrueLitDefs = ppk.allConfigs filter { c =>
    c.defs exists { d => d.iv.isInstanceOf[Value] && (d.iv != Yes || d.cond == Yes) }
  }

  // Configs with defaults that have a condition different from its prompt
  lazy val configsWithCondDerived = ppk.allConfigs filter { c =>
    c.defs exists  { d => c.prompt exists { _.cond != d.cond } }
  } distinct

  // Configs that are NEVER user-selectable
  lazy val configsWithUncondDerived = ppk.allConfigs filter { c =>
    c.prompt.isEmpty
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
    println("Vis. Cond.:          %4d / %4d".format(stats.configsWithVisConds.size, stats.allConfigs.size))
    println("No Vis. Cond.:       %4d / %4d".format(stats.configsWithNoVisConds.size, stats.allConfigs.size))
    // unconditionally derived
    println("Uncond. Derived:     %4d / %4d".format(stats.configsWithUncondDerived.size, stats.allConfigs.size))

    println

    // TODO categories can overlap
    // explicit defaults (soft constraints)
    println("Real Defs:           %4d / %4d".format(stats.configsWithTrueDefs.size, stats.allConfigs.size))

    //    computed (expressions)
    println("  Computed:          %4d / %4d".format(stats.configsWithTrueCompDefs.size, stats.configsWithTrueDefs.size))

    //    literals
    println("  Literals:          %4d / %4d".format(stats.configsWithTrueLitDefs.size, stats.configsWithTrueDefs.size))

    //    conditionally derived
    println("Cond. Derived:       %4d / %4d".format(stats.configsWithCondDerived.size, stats.allConfigs.size))

    println
    // Reverse dependencies
    println("Reverse Deps:        %4d / %4d".format(stats.configsWithRevDeps.size, stats.allConfigs.size))

  }

  def intersectionStats(implicit stats: ASEStatistics) {
    import Intersection._

//    val res =
//      calculatePairWise(Array((stats.configsWithUncondDerived map { _.id }).toSet,
//                      (stats.configsWithTrueDefs map { _.id }).toSet,
//                      (stats.configsWithRevDeps map { _.id }).toSet))
//
//    printPairWiseSizes(res, Array("Uncond.Derived", "w/RealDefaults", "w/RevDeps"))
//
//    println

    val combos =
      calculatePartitions(
        Array((stats.configsWithUncondDerived map { _.id }).toSet,
              (stats.configsWithCondDerived map { _.id }).toSet,
              (stats.configsWithTrueDefs map { _.id }).toSet),
        Array((stats.configsWithTrueDefs map { _.id }).toSet,
              (stats.configsWithRevDeps map { _.id }).toSet),
        Array("w/RealDefaults", "w/RevDep"))

    printComboSizes(combos, Array("Uncond.Derived", "Cond.Derived", "RealDefs."))
  }

  def main(args: Array[String]) {
    println("Parsing exconfig...")
    val k = KConfigParser.parseKConfigFile(
      "/home/shshe/Dev/thorsten/DependencyMiningPaperTools/input/toybox.exconfig"
    )
    //val k = KConfigParser.parseKConfigFile("/home/shshe/Dev/extracts/2.6.32.exconfig")
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