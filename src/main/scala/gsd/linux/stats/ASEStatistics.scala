package gsd.linux.stats
import gsd.linux._

class ASEStatistics(ck: ConcreteKConfig) extends FeatureStatistics(ck) {

  // Feature Kinds
  // menus, menuconfigs, configs, choice

  // Switch features
  lazy val boolType = boolConfigs
  lazy val tristateType = triConfigs
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

  def visibilityStats(implicit stats: ASEStatistics) {
    
  }

  def main(args: Array[String]) {
    println("Parsing exconfig...")
    val k = KConfigParser.parseKConfigFile("/home/shshe/Dev/extracts/2.6.32.exconfig")
    implicit val stats = new ASEStatistics(k)

    println
    representationStats
    println
    groupingStats
  }

}