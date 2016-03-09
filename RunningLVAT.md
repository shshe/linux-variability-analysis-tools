# Introduction #

LVAT is written in Scala and built using [sbt](https://github.com/harrah/xsbt). It is divided into two projects: the base LVAT project, and the fm-translation project.

# Setup #
  * Checkout the base LVAT project:

```
hg clone https://code.google.com/p/linux-variability-analysis-tools/
```

  * Checkout the fm-translation project as the subdirectory _fm-translation/_ under _linux-variability-analysis-tools_:

```
cd linux-variability-analysis-tools
hg clone https://code.google.com/p/linux-variability-analysis-tools.fm-translation/ fm-translation
```

  * Install [sbt](https://github.com/harrah/xsbt) and start the console by running `sbt`.

  * Compile LVAT by running the command `compile` in the sbt console.
  * Compile fm-translation by first switching to the project by running `project fm-translation` then running `compile`.