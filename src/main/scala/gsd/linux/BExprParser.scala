package gsd.linux

import util.parsing.combinator._
import java.io.{PrintStream, InputStream}

object BExprParser extends RegexParsers with PackratParsers with ImplicitConversions {

  override val whiteSpace = """[\t\f ]+""".r
  
  case class BExprGenEqs(genEqString: Map[String, (String, String)],
                         genEqInt: Map[String, (String, Int)],
                         genEqHex: Map[String, (String, String)],
                         genEqVar: Map[String, (String, String)]) {
    lazy val keys = genEqString.keySet ++ genEqInt.keySet ++ genEqHex.keySet ++ genEqVar.keySet
    
    def write(out: PrintStream) {
      for ((gen, (v, value)) <- genEqString)
        out.println("$s %s %s %s".format(gen, v, value))

      for ((gen, (v, value)) <- genEqInt)
        out.println("$i %s %s %s".format(gen, v, value))

      for ((gen, (v, value)) <- genEqHex)
        out.println("$h %s %s %s".format(gen, v, value))

      for ((gen, (v, value)) <- genEqVar)
        out.println("$v %s %s %s".format(gen, v, value))
    }
  }


  abstract sealed class BEq {
    // TODO serializing equality
    // def write(out: PrintStream)
  }
  
  case class BLiteralEq(id: String, value: String) extends BEq
  case class BIntEq(id: String, value: Int) extends BEq
  case class BHexEq(id: String, value: String) extends BEq
  case class BStringEq(id: String, value: String) extends BEq
  case class BVarEq(id: String, value: Int) extends BEq

  case class BExprResult(ids: Set[String],
                         genBools: Set[String],
                         genEqs: BExprGenEqs,
                         expressions: List[BExpr]) {

    lazy val all: Set[String] =
      ids.toSet ++ genBools.toSet ++ genEqs.keys.toList
    
    lazy val generated: Set[String] =
      genBools.toSet ++ genEqs.keys

    lazy val idMap: Map[String, Int] =
      (all.zipWithIndex map { case (id,v) => (id, v+1) }).toMap

    lazy val varMap: Map[Int, String] =
      (idMap map { case (id,v) => (v, id)}).toMap

    def write(out: PrintStream) {
      // Output identifiers
      for (id <- ids)
        out.println("@ " + id)

      // Output generated variables
      for (id <- genBools)
        out.println("$ " + id)

      // Write generated equality expressions
      genEqs.write(out)

      // Output expressions
      for (e  <- expressions)
        out.println(e)
    }
  }

  private lazy val orExpr : PackratParser[BExpr] =
    andExpr ~ rep("""\|[\|]?""".r ~> orExpr) ^^
      {
        case x~ys => (x /: ys){ BOr }
      }
  private lazy val andExpr : PackratParser[BExpr] =
    implExpr ~ rep("&[&]?".r ~> andExpr) ^^
      {
        case x~ys => (x /: ys){ BAnd }
      }
  private lazy val implExpr : PackratParser[BExpr] =
    biimpExpr ~ rep(("->"|"=>") ~> implExpr) ^^
      {
        case x~ys => (x /: ys){ BImplies }
      }
  private lazy val biimpExpr : PackratParser[BExpr] =
    unaryExpr ~ rep(("<->"|"<=>") ~> biimpExpr) ^^
      {
        case x~ys => (x /: ys){ (x,y) => BAnd(BImplies(x,y), BImplies(y,x)) }
      }
  private lazy val unaryExpr : PackratParser[BExpr] =
    ("!" ~> unaryExpr) ^^ BNot | primary
  
  private lazy val primary : PackratParser[BExpr] =
    """\w+""".r ^^
      {
        case "1" => BTrue
        case "0" => BFalse
        case n => BId(n)
      } |
    "(" ~> orExpr <~ ")"

  def parseBExpr(str: String) = succ(parseAll(orExpr, str))

  def parseBExprResult(in: InputStream): BExprResult =
    parseBExprResult(new java.util.Scanner(in))

  /**
   * More efficient to use scanner to split by line instead of relying solely
   * on parser combinators.
   */
  def parseBExprResult(s: java.util.Scanner) : BExprResult = {
    import collection.mutable.ListBuffer
    
    val ids  = new ListBuffer[String]
    val genBools = new ListBuffer[String]
    val genEqString = new ListBuffer[(String, (String, String))]
    val genEqInt = new ListBuffer[(String, (String, Int))]
    val genEqHex = new ListBuffer[(String, (String, String))]
    val genEqVar = new ListBuffer[(String, (String, String))]
    val exprs = new ListBuffer[BExpr]

    val eqRegEx = """\$(.) (\w+) (\w+) (\w+)""".r

    while (s.hasNextLine) {
      val line = s.nextLine
      if (line.trim.isEmpty) {} //do nothing
      else if (line.startsWith("@")) ids += line.substring(1).trim
      else if (line.startsWith("$")) {
        line match {
          case eqRegEx(t, gen, variable, value) =>
            t match {
              // Generated string equality
              case "s" =>
               genEqString += ((gen, (variable, value)))

              // Generated integer equality
              case "i" =>
                genEqInt += ((gen, (variable, value.toInt)))

              // Generated hex equality
              case "h" =>
                genEqHex += ((gen, (variable, value)))

              // Generated variable equality
              case "v" =>
                genEqVar += ((gen, (variable, value)))

              // Regular generated variable
              case _ =>
                sys.error("Unrecognized generated equality type: " + line)
            }
          // Regular generated variable
          case _ =>
            genBools += line.substring(1).trim
        }
      }
      else exprs += parseBExpr(line)
    }
    s.close()
    BExprResult(ids.toSet,
                genBools.toSet,
                BExprGenEqs(genEqString.toMap, genEqInt.toMap, genEqHex.toMap, genEqVar.toMap),
                exprs.toList filter { _ != BTrue })
  }

  def parseBExprResult(file: String) : BExprResult =
    parseBExprResult(new java.util.Scanner(new java.io.File(file)))


  protected def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => sys.error(x.toString)
  }

}
