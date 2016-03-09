Propositional formulas in Conjunctive Normal Form (CNF) are available as .dimacs in [this repository](https://code.google.com/p/linux-variability-analysis-tools/source/checkout?repo=formulas).The translation is based on the [Kconfig semantics](http://eng.uwaterloo.ca/~shshe/kconfig_semantics.pdf).

# Dimacs header #
The .dimacs files have a header that maps variables to features such as the one below:
```
c 1 a
c 2 b
c 3$ _x
```

A `c` indicates a dimacs comment, the number indicates the variable and the text that follows is the feature name. A `$` indicates that the variable is generated as part of the CNF translation process, and is not a feature of the model.