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

  // Post-processed ConcreteKConfig (removed inherited)
  lazy val ppk = PostProcess.removeInherited(ck)
  lazy val apk = ppk.toAbstractKConfig

}

