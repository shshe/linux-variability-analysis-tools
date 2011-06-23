package gsd.linux

import util.parsing.combinator._
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq
import java.io.InputStream

object BExprParser extends RegexParsers with PackratParsers with ImplicitConversions {

  override val whiteSpace = """[\t\f ]+""".r

  case class BExprResult(ids: List[String], 
                         generated: List[String],
                         expressions: List[BExpr]) {

    lazy val all: List[String] = ids ::: generated

    lazy val idMap: Map[String, Int] =
      ((ids ::: generated).zipWithIndex map { case (id,v) => (id, v+1) }).toMap

    lazy val varMap: Map[Int, String] =
      (idMap map { case (id,v) => (v, id)}).toMap
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
    val gens = new ListBuffer[String]
    val exprs = new ListBuffer[BExpr]

    while (s.hasNextLine) {
      val line = s.nextLine
      if (line.trim.isEmpty) {} //do nothing
      else if (line.startsWith("@")) ids += line.substring(1).trim
      else if (line.startsWith("$")) gens += line.substring(1).trim
      else exprs += parseBExpr(line)
    }
    s.close
    BExprResult(ids.toList, gens.toList, exprs.toList filter { _ != BTrue })
  }

  def parseBExprResult(file: String) : BExprResult =
    parseBExprResult(new java.util.Scanner(new java.io.File(file)))


  protected def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => sys.error(x.toString)
  }

}
