package gsd.linux

import collection.mutable.ListBuffer

class TristateTranslation(k: AbstractKConfig) {
  import TExpr._

  object IdGen {
    val prefix = "_X"
    var i = 0
    def next = { i+=1; prefix + i }
    def allIds = (1 to i).map { prefix + _ }.toList
  }

  def generated: List[String] =
    (IdGen.allIds map { _ + "_1" }) ::: (IdGen.allIds map { _ + "_2"})

  def identifiers: List[String] =
    k.identifiers.toList ::: IdGen.allIds

  def size: Int = identifiers.size

  /*
   * Var i (odd) represents identifier x_1, Var i+1 (even) represents x_2.
   */
  def idMap: Map[String, Int] =
    Map() ++ {
      (identifiers flatMap
              { id => List(id + "_1", id + "_2") }).zipWithIndex map
                     { case (id,i) => (id, i + 1) }
    }


  def varMap: Map[Int, String] =
    Map() ++ (idMap map { case (id,v) => (v,id) })


  def interpret(model: Array[Int]): List[(String, Int)] = {
    val varMap = Map() ++ (idMap map { case (id,v) => (v, id) })
    val result = new ListBuffer[(String, Int)]

    // Iterate only through identifiers in the Kconfig (ignoring generated)
    for (i <- 0 until model.length by 2) {
      val key = i + 1

      assert(math.abs(model(i)) == key, model(i) + " != " + key)
      val i1 = model(i) > 0
      val i2 = model(i+1) > 0
      val id: String = varMap(key).slice(0, varMap(key).length - 2) //FIXME drop _1 suffix
      val state = (i1, i2) match {
        case (true, true)   => 2
        case (true, false)  => 1
        case (false, false) => 0
        case (false, true)  => error("(0,1) state should never be in a model!")
      }
      result += Tuple2(id, state)
    }

  result.toList
  }

  /**
   * Stateful: Changes identifiers in IdGen
   */
  lazy val translate: List[BExpr] =
    (translateNotSimplified map { _.simplify }) filterNot { _ == BTrue }

  lazy val translateNotSimplified: List[BExpr] =
    ((k.configs flatMap translate) filterNot { _ == BTrue}) ::: {
      // Disallow mod state from Boolean configs

      k.configs filter
              { _.ktype == KBoolType } map
              { _.id } map
              { id => BId(id + "_1") implies BId(id + "_2") }
    } ::: {
      // Disallow mod state from String, Int and Hex configs

      k.configs filter
              { t => t.ktype == KIntType || t.ktype == KHexType || t.ktype == KStringType } map
              { _.id } map
              { id => BId(id + "_1") implies BId(id + "_2") }
    } ::: {
      // Disallow (0,1) state
      // Ignore generated identifiers because they have equivalence

      k.identifiers.toList map { id => BId(id + "_1") | !BId(id + "_2") }
    }

  /**
   * FIXME no ranges.
   * Always introducing new variable for reverse dependency expression.
   */
  def translate(c: AConfig): List[BExpr] = c match {
    case AConfig(id, t, inh, pro, defs, rev, ranges) =>

      val rds = rev map { r => (toTExpr(r), TId(IdGen.next)) }
      //FIXME converting every reverse dependency to new id
      val rdsExpr = ((TNo: TExpr) /: rds){ case (x,(_, id)) => x | id }
      val rdsId = if (rds == TNo) TNo else TId(IdGen.next)

      def t(rest: List[Default], prev: List[Default]): List[BExpr] =
        rest match {
          case Nil => Nil

          case (h@Default(e,cond))::tail =>
            val ante = ((toTExpr(pro) eq TNo) /: prev){ (x,y) =>
              x & (toTExpr(y.cond) eq TNo)
            } & (toTExpr(cond) > TNo)

            // Handle default y quirk
            val tex = if (e == Yes) toTExpr(cond) else toTExpr(e)

            (ante implies (TId(id) eq (tex | rdsId))) :: t(tail, h::prev)
        }

    (rds map { case (e, id) => id eq e }) ::: // reverse dependency equivalence
    (rdsId eq rdsExpr) :: // reverse dependency equivalence
            (rdsId <= TId(id)) ::  // reverse dependency lower bound
            (TId(id) <= toTExpr(inh)) ::  // inherited upper bound
            t(defs, Nil)
  }

}