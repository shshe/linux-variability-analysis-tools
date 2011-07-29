package gsd.linux.stats
import gsd.linux._
import java.io.PrintStream

class ASEStatistics(val ck: ConcreteKConfig)
  extends FeatureStatistics(ck) with VisibilityStatistics {

  // Feature Kinds
  // menus, menuconfigs, configs, choice

  // Switch features
  lazy val boolType = boolConfigs
  lazy val tristateType = triConfigs

  // Post-processed ConcreteKConfig (removed inherited and depends on)
  lazy val ppk = ASEStatistics.removeInheritedAndDependsOn(ck)
  lazy val apk = ppk.toAbstractKConfig

  //Post-processed ConcreteKconfig (removed inherited)
  lazy val ipk = ASEStatistics.removeInherited(ck)
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
      { cj contains } filterNot
      { _ == Yes } mkConjunction
  }

  def rewriteProperties(ck: ConcreteKConfig)(f: (CConfig, Property) => KExpr)
      : ConcreteKConfig = {
    val strategy =
      everywheretd {
        rule {
          case config@CConfig(_,_,_,_,inh,pros,ds,selects,rngs,_,_) =>
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
