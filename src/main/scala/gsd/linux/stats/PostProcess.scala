package gsd.linux.stats

import gsd.linux._

object PostProcess {

  import KExprList._
  import org.kiama.rewriting.Rewriter._

  /**
   * Removes a conjunction from the condition of a property
   */
  def removeFromCondition(cond: KExpr, conj: KExpr): KExpr = {
    val cj = conj.splitConjunctions
    cond.splitConjunctions filterNot
      { cj contains } filterNot
      { _ == Yes } mkConjunction
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
      removeFromCondition(p.cond, config.inherited)
    }

  def removeParentFromDependsOn(ck: ConcreteKConfig): ConcreteKConfig = {
    val hi = Hierarchy.mkConfigMap(ck)
    val strategy =
      everywheretd {
        rule {
          case c: CConfig =>
            val parentId = Id(hi(c).id)
            c.copy (depends = c.depends map { d =>
              DependsOn(removeFromCondition(d.cond, parentId))
            })
        }
      }
    rewrite(strategy)(ck)
  }

}


