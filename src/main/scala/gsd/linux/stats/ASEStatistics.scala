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

  // Explicitly defined prompt (i.e. condition not propagated by an inherited
  // or depends on condition)
  lazy val configsWithVisConds = ppk.allConfigs filter { c =>
    c.prompt exists { _.cond != Yes }
  }

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
    println("Visibility Conditions: %d / %d".format(stats.configsWithVisConds.size, stats.allConfigs.size))

    stats.configsWithVisConds foreach { c => println(c.prompt) }

    // explicit defaults (soft constraints)

    //    computed (expressions)

    //    literals

    //    conditionally derived


    // unconditionally derived
    
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
  }

}