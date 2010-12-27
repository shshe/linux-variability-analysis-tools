package gsd.linux.stats

import java.io.PrintStream
import gsd.linux.KConfigParser

object ASEStatisticsMain {

  def representationStats(stats: ASEStatistics)(out: PrintStream) {
    out.println("Bool:   " + stats.boolType.size)
    out.println("Tri:    " + stats.tristateType.size)

    out.println
    out.println("Int:    " + stats.intConfigs.size)
    out.println("Hex:    " + stats.hexConfigs.size)
    out.println("String: " + stats.stringConfigs.size)

    out.println
    out.println("Menus:  " + stats.menus.size)
  }

  def groupingStats(stats: ASEStatistics)(out: PrintStream) {
    out.println("Menus:        " + stats.menus.size)
    out.println("Menuconfigs:  " + stats.menuconfigs.size)

    out.println
    out.println("XOR:          " + stats.xorGroups.size)
    out.println("OR:           " + stats.orGroups.size)
    out.println("MUTEX:        " + stats.mutexGroups.size)
  }

  def dependencyStats(stats: ASEStatistics)(out: PrintStream) {
    // visibility conditions
    out.println("Vis. Cond.:      %4d / %4d".format(
      stats.configsWithVisConds.size, stats.allConfigs.size))
    out.println("No Vis. Cond.:   %4d / %4d".format(
      stats.configsWithNoVisConds.size, stats.allConfigs.size))
    // unconditionally derived
    out.println("Uncond. Derived: %4d / %4d".format(
      stats.configsWithUncondDerived.size, stats.allConfigs.size))

    out.println

    // TODO categories can overlap
    out.println("Real Defs:       %4d / %4d".format(
      stats.configsWithTrueDefs.size, stats.allConfigs.size))
    out.println("Cond. Derived:   %4d / %4d".format(
      stats.configsWithCondDerived.size, stats.allConfigs.size))

    out.println
    out.println("Reverse Deps:    %4d / %4d".format(
      stats.configsWithRevDeps.size, stats.allConfigs.size))

    out.println
    out.println("Def (lit):  %4d / %4d".format(
      stats.defaultsValued.size, stats.defaults.size))
    out.println("Def (comp): %4d / %4d".format(
      stats.defaultsComputed.size, stats.defaults.size))

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
    if (args.size < 2) {
      System.err.println("Usage: ASEStatisticsMain [-r|-g|-d] <input-exconfig-file> [<output-file>]")
      System exit 1
    }


    val k = KConfigParser.parseKConfigFile(args(1))
    val stats = new ASEStatistics(k)
    val out = if (args.size > 2) new PrintStream(args(2))
              else System.out

    args(0) match {
      case "-r" => representationStats(stats)(out)
      case "-g" => groupingStats(stats)(out)
      case "-d" => dependencyStats(stats)(out)
    }
  }

}

