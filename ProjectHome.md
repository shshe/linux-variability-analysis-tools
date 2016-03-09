The Linux Variability Analysis Tools (LVAT) is a tool suite written in Scala for analysing the Linux Kconfig model. LVAT is written and maintained by [Steven She (mintcoffee)](http://www.eng.uwaterloo.ca/~shshe/) who is part of the [Generative Software Development Lab](http://gsd.uwaterloo.ca).

## Library ##

Snapshots of LVAT are published in the Sonatype OSS snapshot repository:

  * Repository: https://oss.sonatype.org/content/repositories/snapshots/
  * groupId: com.googlecode.linux-variability-analysis-tools
  * artifactId: lvat\_2.9.0-1
  * version: 1.0-SNAPSHOT

## Tools ##

Currently, there are three tools available in the suite:

  * **Propositional formula extractor**
  * **Kconfig statistics**
  * **Feature model translator**

## Extracts ##

LVAT relies on Kconfig model extracts (.exconfig). Extracts for Linux x86 v2.6.12 through v.2.6.32 are available from the [extracts repository](https://code.google.com/p/linux-variability-analysis-tools/source/checkout?repo=extracts).

## Kconfig Intermediate Format Extractor ##

If you're interested in other architectures or versions of Linux, the extractor source is available from the [exconfig repository](https://code.google.com/p/linux-variability-analysis-tools/source/checkout?repo=exconfig).

## Feature Model Translator ##

The translator takes a .exconfig file and outputs the a feature model in Clafer syntax. This translator is responsible for the Clafer model [available from the GSD website](http://gsd.uwaterloo.ca/feature-models-in-the-wild). The code should be considered _pre-release_ and is available from the [fm-translation repository](https://code.google.com/p/linux-variability-analysis-tools/source/checkout?repo=fm-translation).