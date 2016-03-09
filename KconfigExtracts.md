# Existing Kconfig Extracts #

Existing Kconfig extracts are available in this repository: [Kconfig extracts repository](https://code.google.com/p/linux-variability-analysis-tools/source/browse?repo=extracts)

# Extracting Kconfig #

LVAT does not operate directly on the Kconfig language - it relies on a C program written with the Kconfig infrastructure to convert a Kconfig specification to a format that's compatible with LVAT.

There are [two extractors available](https://code.google.com/p/linux-variability-analysis-tools/source/browse/?repo=exconfig):

  * **exconf** - extracts Kconfig to a plain text format. Compatible with Kconfig written upto Linux 3.0 (prior to the introduction of the _visible_ keyword on menus). No longer maintained.
  * **protoconf** - extract Kconfig to a Protocol Buffer. Should be compatible with all Kconfig files.

It is recommended to use protoconf, however, exconf is useful if you want to read a Kconfig specification as a flat text file.

# Reading Kconfig extracts #

The following Scala code snippet is used to parse a Kconfig extract:
```
import gsd.linux._

val k: ConcreteKConfig = KConfigParser.parseKConfigFile("<path to Kconfig extract>")

// Print the number of menus
println(k.menus.size)

// Print the names of all the menus
k map (_.prompt.text) foreach println

// Print the prompt conditions for the BT config
k.configMap("BT").prompt map (_.cond) foreach println
```