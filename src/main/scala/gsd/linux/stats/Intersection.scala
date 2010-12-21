package gsd.linux.stats

import gsd.linux.Combinations

object Intersection {

  /**
   * @param in An array of n sets
   * @return An n x n array of sets such that the A[i][j] is the intersection of the ith and jth sets.
   */
  def calculatePairWise[T](in: Array[Set[T]]): Array[Array[Set[T]]] = {
    val result = Array.ofDim[Set[T]](in.size, in.size)
    for {
      i <- 0 until in.size
      j <- 0 until in.size
    } {
      if (i == j) result(i)(j) = in(i)
      else if (j < i) result(i)(j) = result(j)(i)
      else result(i)(j) = in(i) & in(j)
    }
    result
  }

  def printPairWiseSizes[T](res: Array[Array[Set[T]]], labels: Array[String]) {
    val widths = labels map { _.length }
    val largest = widths reduceLeft { math.max }

    //Print column itemLabels
    print(("%" + largest + "s  ").format(" "))
    for (i <- 0 until labels.size)
      print(("%" + widths(i) + "s  ").format(labels(i)))
    println

    for (i <- 0 until res.length) {
      print(("%" + largest + "s  ").format(labels(i)))
      for (j <- 0 until res(i).length)
        print(("%" + widths(j) + "s  ").format(res(i)(j).size))

      println
    }
  }

  def calculatePartitions[T](bases: Array[Set[T]], items: Array[Set[T]], itemLabels: Array[String])
      : Array[Array[(String, Set[T])]] = {

    bases map { base =>
      assert(itemLabels.size == items.size)

      val itemsWithLabels: Array[(String, Set[T])] = itemLabels zip items
      val combos = Nil :: Combinations.all((0 until items.size).toList)

      combos map {

        case Nil =>
          ("None", items.foldLeft (base) { _ -- _ })

        case combo =>

        val label = combo map { itemLabels(_) } mkString "&"

        val selected = combo map { items(_) }
        val remaining = ((0 until items.size).toSet -- combo) map { items(_) }

        val result =
          base & (selected reduceLeft { _ & _ }) --
            (remaining.foldLeft (Set[T]()) { _ union _ })
        (label, result)
      } toArray
    }
  }

  def printComboSizes[T](ins: Array[Array[(String, Set[T])]], baseLabels: Array[String]) {
    val widths = ins(0) map { _._1 } map { _.length }
    val largest = baseLabels map { _.length } reduceLeft { math.max }

    print(("%" + largest + "s  ").format(" "))
    (ins(0) map { _._1 }).zipWithIndex foreach {
      case (l,i) => print(("%" + widths(i) + "s  ").format(l))
    }

    println

    for ((baseLabel, in) <- baseLabels zip ins) {
      print(("%" + largest + "s  ").format(baseLabel))
      (in map { _._2 }).zipWithIndex foreach {
        case (s,i) => print(("%" + widths(i) + "s  ").format(s.size))
      }
      println
    }
  }

}