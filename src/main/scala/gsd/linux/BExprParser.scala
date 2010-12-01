package gsd.linux

import util.parsing.combinator._
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

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

  val nl = """[\r]?\n""".r
  val ids  = rep("@" ~> """\w+""".r <~ rep1(nl))
  val gens = rep("$" ~> """\w+""".r <~ rep1(nl))

  private lazy val exprresults = ids ~ gens ~
    (rep(orExpr <~ rep1(nl)) ^^  { _ filterNot { _ == BTrue } }) ^^ BExprResult


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

  def parseBExpr(reader: PagedSeq[Char]) =
    succ(parseAll(exprresults, reader))

  def parseBExprFile(file: String) : BExprResult =
    parseBExpr(PagedSeq fromFile file)

  protected def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => error(x.toString)
  }

}
